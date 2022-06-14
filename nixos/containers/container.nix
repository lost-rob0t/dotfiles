{ config, lib, pkgs, ... }:

{
  virtualisation.oci-containers = {
  backend = "podman";
  containers = {
    "starintel-db" = {
      image = "";
      ports = [ "5984:5984" ];
      environment = {
        COUCHDB_USER = "admin";
        COUCHDB_PASSWORD = "password";
      };
      #volumes = [ "/data/nexus/dbdata/:/opt/couchdb/data/" ];
      autoStart = true;
    };
    #"test" = {
    #  image = "wordpress";
    #  ports = [ "80:80" ];
    #  #volumes = [ "/data/nexus/dbdata/:/opt/couchdb/data/" ];
    #  autoStart = true;
    #};
  };
};
}
