"""Hermetic module runtime/config checks; deliberately never invoke Docker."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

metadata, prepare, infrastructure = sys.argv[1:]
config = json.loads(Path(metadata).read_text())
root = Path(config['root'])
assert str(root) == '/build/starintel-fixture'
for name in ('couchdb', 'clouseau', 'rabbitmq', 'valkey'):
    (root / 'data' / name).mkdir(parents=True)
root.chmod(0o700)
secret_dir = root / 'config/secrets'
secret_dir.mkdir(parents=True)
for name in ('couchdb_password', 'couchdb_secret', 'erlang_cookie', 'rabbitmq_password', 'valkey_password', 'auth_pepper', 'auth_bootstrap_secret', 'initial_password'):
    file = secret_dir / name
    file.write_text('hermetic-fixture-value')
    file.chmod(0o600)
subprocess.run([prepare], check=True)
init = (root / 'config/init.lisp').read_text()
for setting in ('*auth-pepper*', '*auth-bootstrap-secret*', '*auth-initial-password*', '*valkey-lease-password-file*'):
    assert setting in init
assert '(environment-secret "STAR_AUTH_PEPPER" "STAR_AUTH_PEPPER_FILE")' in init
for name in ('init.lisp', 'quasar-init.lisp'):
    file = root / 'config' / name
    assert file.is_file() and file.stat().st_mode & 0o777 == 0o600
    file.write_text('; operator-owned sentinel\n')
subprocess.run([prepare], check=True)
for name in ('init.lisp', 'quasar-init.lisp'):
    assert (root / 'config' / name).read_text() == '; operator-owned sentinel\n'
(secret_dir / 'auth_bootstrap_secret').unlink()
subprocess.run([prepare], check=True)
file = secret_dir / 'couchdb_password'
private_target = secret_dir / 'managed-couchdb-password'
file.rename(private_target)
file.symlink_to(private_target)
subprocess.run([prepare], check=True)
file.unlink()
private_target.rename(file)
file.chmod(0o644)
assert subprocess.run([prepare], stdout=subprocess.PIPE, stderr=subprocess.PIPE).returncode != 0
file.chmod(0o600)
file.unlink()
assert subprocess.run([prepare], stdout=subprocess.PIPE, stderr=subprocess.PIPE).returncode != 0

script = Path(infrastructure).read_text()
compose_path = re.search(r'/nix/store/[^\s]+starintel-compose.json', script).group()
compose = json.loads(Path(compose_path).read_text())
assert set(compose['services']) == {'couchdb', 'clouseau', 'rabbitmq', 'valkey'}
assert 'volumes' not in compose
assert compose['name'] == 'starintel-' + hashlib.sha256(str(root).encode()).hexdigest()[:16]
for name, service in compose['services'].items():
    assert service['pull_policy'] == 'never'
    assert service['restart'] == 'on-failure:3'
    for volume in service['volumes']:
        assert volume['type'] == 'bind'
        assert volume['source'] == f'{root}/data/{name}'
        assert volume['bind']['create_host_path'] is False
    assert all(port.startswith('127.0.0.1:') for port in service.get('ports', []))
for secret in compose['secrets'].values():
    assert secret['file'].startswith(f'{root}/config/secrets/')
assert 'hermetic-fixture-value' not in json.dumps(compose)
assert '--wait' in script and 'docker load --input' in script
assert '--volumes' not in script and 'docker pull' not in script
for unit in config['services'].values():
    assert not unit.get('Install', {}).get('WantedBy', [])
    assert 'starintel.target' in unit['Unit']['PartOf']
target = config['targets']['starintel']
assert target['Install']['WantedBy'] == ['default.target']
assert set(target['Unit']['Wants']) == {name + '.service' for name in config['services']}
assert target['Unit']['Upholds'] == target['Unit']['Wants']
assert 'Requires' not in target['Unit']
for name in ('starintel-prepare', 'starintel-infrastructure'):
    assert config['services'][name]['Service']['Restart'] == 'on-failure'
router = config['services']['starintel-router']['Service']['ExecStart'][0]
assert '--pubAddress=tcp://127.0.0.1:6000' in router
assert '--apiAddress=tcp://127.0.0.1:6001' in router
server = config['services']['starintel-server']['Service']
assert 'ExecStartPost' in server
assert f'VALKEY_PASSWORD_FILE={secret_dir}/valkey_password' in server['Environment']
assert not any(entry.startswith('VALKEY_PASSWORD=') for entry in server['Environment'])
print('StarIntel module configuration, init preservation and secret checks passed')
