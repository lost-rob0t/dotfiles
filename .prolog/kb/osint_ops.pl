% Durable OSINT operations knowledge, learned from verified passive recon runs.
% Provenance: evidence files live in hackmode/<op>/ (operation.org, recon/, vuln/).
% Every fact carries its verification date; raw evidence stays in the op dirs.

% --- kolektiva.social operation (started 2026-11-26) ---

ops_target('kolektiva.social').
ops_target('kolektiva.media').
ops_target('newday.host').
ops_target('oakcel.org').

ops_op_dir('kolektiva-social', '/home/unseen/.dotfiles/hackmode/kolektiva-social').

% Infrastructure cluster: shared Cloudflare NS pair ties the three zones together.
ops_cluster_member('kolektiva.social',  shared_ns('curt.ns.cloudflare.com', 'zelda.ns.cloudflare.com')).
ops_cluster_member('kolektiva.media',   shared_ns('curt.ns.cloudflare.com', 'zelda.ns.cloudflare.com')).
ops_cluster_member('newday.host',       shared_ns('curt.ns.cloudflare.com', 'zelda.ns.cloudflare.com')).

% Registry facts (RDAP + ARIN whois, verified 2026-11-26).
ops_domain_fact('kolektiva.social', registrar('Cloudflare, Inc.')).
ops_domain_fact('kolektiva.social', registered(2020, 08, 20)).
ops_domain_fact('kolektiva.social', transferred(2021, 04, 02)).
ops_domain_fact('kolektiva.social', expires(2027, 08, 20)).
ops_domain_fact('kolektiva.social', dnssec(alg13, keyTag2371)).

ops_domain_fact('kolektiva.media', registrar('Cloudflare, Inc')).
ops_domain_fact('kolektiva.media', registered(2020, 04, 17)).
ops_domain_fact('kolektiva.media', transferred(2022, 03, 29)).

ops_domain_fact('newday.host', registrar('Cloudflare, Inc.')).
ops_domain_fact('newday.host', registered(2015, 05, 28)).
ops_domain_fact('newday.host', expires(2027, 05, 28)).

% IP space: SPF ip4:64.4.175.75/76 resolves into the KPFA /24 (ARIN NET-64-4-175-0-1),
% with OAKCEL (Oakland 501c3, EIN 87-4208362) holding the reassigned /26.
ops_ip_fact('64.4.175.64/26', owner('OAKCEL', 'Oakland Center for Ethical Livelihood')).
ops_ip_fact('64.4.175.0/24',  owner('KPFA', 'Pacifica radio Berkeley CA')).
ops_ip_fact('64.4.175.0/24',  arin_handle('NET-64-4-175-0-1', regdate(2019, 08, 12))).
ops_ip_fact('64.4.175.64/26', arin_handle('NET-64-4-175-64-1', reassigned(2023, 02, 17))).
ops_ip_fact('64.4.175.67',    ptr('cloud.newday.host')).
ops_ip_fact('64.4.175.75',    ptr('mail.newday.host')).
ops_ip_fact('64.4.175.76',    ptr('mx.newday.host')).

% Mail chain: kolektiva DMARC reports route to newday.host; SPF allows OAKCEL/KPFA space.
ops_mail_fact('kolektiva.social', darc_reporting('tlsrpt@newday.host')).
ops_mail_fact('kolektiva.social', dmarc_policy(reject)).
ops_mail_fact('kolektiva.social', spf_allows('64.4.175.75')).
ops_mail_fact('kolektiva.social', spf_allows('64.4.175.76')).
ops_mail_fact('newday.host',      spf_include('spf.smtp2go.com')).

% Software posture (Wayback nodeinfo / archived homepage, verified 2026-11-26).
ops_software('kolektiva.social', mastodon('4.5.8'), asof(2026, 05)).
ops_software('kolektiva.social', users(41642)).
ops_software('kolektiva.social', statuses(4338941)).
ops_software('kolektiva.media', peertube('7.3.0'), asof(2026, 10, 02)).

% Vulnerability posture (upstream advisories, no in-scope testing performed).
ops_vuln('kolektiva.social', mastodon, critical('GHSA-288h-6j8h-6x5j', ap_identifier_impersonation, fixed_in('4.5.20'))).
ops_vuln('kolektiva.social', mastodon, unpatched('GHSA-cf49-73r7-rxj6', jsonld_dos)).
ops_vuln('kolektiva.media',  peertube, critical('GHSA-pqr4-34h8-g39x', unauth_sqli_admin_takeover, fixed_in('8.1.6'))).
ops_vuln('kolektiva.media',  peertube, critical('GHSA-g9p4-f7h8-hc86', ap_update_video_takeover, fixed_in('8.2.2'))).
ops_vuln('kolektiva.media',  peertube, critical('GHSA-37jf-59fg-9hpr', actor_rebinding, fixed_in('8.2.4'))).
ops_vuln('kolektiva.media',  peertube, critical('GHSA-jwcf-xj6w-xqm5', federated_svg_storyboard_xss, fixed_in('8.3.1'))).
% All 12 of PeerTube's 2026 advisories affect 7.3.0; no 7.3.x patch releases exist.
ops_software_note('kolektiva.media', peertube_7_3_0_is_final_of_line).

% Recurring pattern across the two codebases (verified finding, reusable).
ops_pattern(activitypub_identity_binding_failures,
            'sender/owner and redirect-host identity checks missing in both Mastodon 4.5.8 and PeerTube 7.3.0 federation paths').

% Governance / contact facts (archived pages, Wayback 2021-06-13 and 2026-10-02).
ops_governance('kolektiva.social', admin_contact('admin@kolektiva.social')).
ops_governance('kolektiva.social', moderation_note(anti_doxing_coc_clause)).
ops_governance('kolektiva.social', funding('itsgoingdown.org/donate')).
ops_governance('kolektiva.media',  contact_email('kolektiva@riseup.net')).
ops_governance('kolektiva.social', blocked_federated_servers(approx(104), asof(2021, 06))).

% Ingest leads and gates (state, not inference).
ops_ingest_lead('kolektiva', wayback_handle_corpus(9970, 'hackmode/kolektiva-social/recon/wayback-urls.txt')).
ops_ingest_lead('kolektiva', fediwatch_timeline_token_in('starintel-ops/corpus/targets/starintel-targets.org', operator_gate_required)).
ops_ingest_lead('kolektiva', discourse_archive('discourse.kolektiva.social', 'Wayback pass pending')).

% Query: critical vulns for a target (target, software, advisory, what, fixed-in).
ops_critical(Target, Software, Ghsa, What, FixedIn) :-
    ops_vuln(Target, Software, critical(Ghsa, What, fixed_in(FixedIn))).

% Query: everything known about a domain (registry + mail + governance).
ops_domain_profile(Domain, Fact) :-
    ops_domain_fact(Domain, Fact).
ops_domain_profile(Domain, mail(Fact)) :- ops_mail_fact(Domain, Fact).
ops_domain_profile(Domain, gov(Fact)) :- ops_governance(Domain, Fact).
ops_domain_profile(Domain, sw(SoftwareTerm, AsOf)) :- ops_software(Domain, SoftwareTerm, AsOf).

% Query: members of the cluster a target belongs to (target, member, reason).
ops_cluster(Target, Member, Why) :-
    ops_cluster_member(Target, Why),
    ops_cluster_member(Member, Why).
ops_cluster(Target, Target, Why) :-
    \+ ops_cluster_member(Target, _),
    ops_cluster_member(Target2, Why),
    Target \= Target2.

% Query: targets with a pre-auth critical reachable via federation.
ops_federation_exposed(Target) :-
    ops_vuln(Target, _, critical(_, ap_identifier_impersonation, _)).
ops_federation_exposed(Target) :-
    ops_vuln(Target, _, critical(ap_update_video_takeover, _, _)).
