{ pkgs, ... }:

{
  systemd.services.gitlab-search-service = {
    description = "GitLab search service";
    script = 
      let service = pkgs.haskellPackages.callCabal2nix "gitlab-search-service" ./. {};
      in "${service}/bin/gitlab-search-service";
    wantedBy = [ "multi-user.target" ];
  };
}
