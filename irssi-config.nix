{ config, lib, pkgs, ... }:
rec {
  environment.etc."irssi/config".text = ''
    servers = (
      {
        address = "10.0.10.10";
        chatnet = "ZNC";
        port = "26667";
        use_ssl = "no";
        ssl_verify = "no";
        autoconnect = "yes";
      }
    );

    chatnets = {
      ZNC = {
        type = "IRC";
        nick = "NeQuissimus";
        username = "nequi";
        realname = "Tim Steinbach";
      };
    };

    channels = (
      { name = "#nixos"; chatnet = "ZNC"; autojoin = "yes"; }
    );

    aliases = {
      ATAG = "WINDOW SERVER";
      ADDALLCHANS = "SCRIPT EXEC foreach my \\$channel (Irssi::channels()) { Irssi::command(\"CHANNEL ADD -auto \\$channel->{name} \\$channel->{server}->{tag} \\$channel->{key}\")\\;}";
      B = "BAN";
      BACK = "AWAY";
      BANS = "BAN";
      BYE = "QUIT";
      C = "CLEAR";
      CALC = "EXEC - if command -v bc >/dev/null 2>&1\\; then printf '%s=' '$*'\\; echo '$*' | bc -l\\; else echo bc was not found\\; fi";
      CHAT = "DCC CHAT";
      DATE = "TIME";
      DEHIGHLIGHT = "DEHILIGHT";
      DESCRIBE = "ACTION";
      DHL = "DEHILIGHT";
      EXEMPTLIST = "MODE $C +e";
      EXIT = "QUIT";
      GOTO = "SCROLLBACK GOTO";
      HIGHLIGHT = "HILIGHT";
      HL = "HILIGHT";
      HOST = "USERHOST";
      INVITELIST = "MODE $C +I";
      J = "JOIN";
      K = "KICK";
      KB = "KICKBAN";
      KN = "KNOCKOUT";
      LAST = "LASTLOG";
      LEAVE = "PART";
      M = "MSG";
      MUB = "UNBAN *";
      N = "NAMES";
      NMSG = "^MSG";
      P = "PART";
      Q = "QUERY";
      RESET = "SET -default";
      RUN = "SCRIPT LOAD";
      SAY = "MSG *";
      SB = "SCROLLBACK";
      SBAR = "STATUSBAR";
      SIGNOFF = "QUIT";
      SV = "MSG * Irssi $J ($V) - http://www.irssi.org";
      T = "TOPIC";
      UB = "UNBAN";
      UMODE = "MODE $N";
      UNSET = "SET -clear";
      W = "WHO";
      WC = "WINDOW CLOSE";
      WG = "WINDOW GOTO";
      WJOIN = "JOIN -window";
      WI = "WHOIS";
      WII = "WHOIS $0 $0";
      WL = "WINDOW LIST";
      WN = "WINDOW NEW HIDDEN";
      WQUERY = "QUERY -window";
      WW = "WHOWAS";
      1 = "WINDOW GOTO 1";
      2 = "WINDOW GOTO 2";
      3 = "WINDOW GOTO 3";
      4 = "WINDOW GOTO 4";
      5 = "WINDOW GOTO 5";
      6 = "WINDOW GOTO 6";
      7 = "WINDOW GOTO 7";
      8 = "WINDOW GOTO 8";
      9 = "WINDOW GOTO 9";
    };

    statusbar = {

      items = {

        barstart = "{sbstart}";
        barend = "{sbend}";

        topicbarstart = "{topicsbstart}";
        topicbarend = "{topicsbend}";

        time = "{sb $Z}";
        user = "{sb {sbnickmode $cumode}$N{sbmode $usermode}{sbaway $A}}";

        window = "{sb $winref:$tag/$itemname{sbmode $M}}";
        window_empty = "{sb $winref{sbservertag $tag}}";

        prompt = "{prompt $[.15]itemname}";
        prompt_empty = "{prompt $winname}";

        topic = " $topic";
        topic_empty = " Irssi v$J - http://www.irssi.org";

        lag = "{sb Lag: $0-}";
        act = "{sb Act: $0-}";
        more = "-- more --";
      };

      default = {

        window = {

          disabled = "no";
          type = "window";
          placement = "bottom";
          position = "1";
          visible = "active";

          items = {
            barstart = { priority = "100"; };
            time = { };
            user = { };
            window = { };
            window_empty = { };
            lag = { priority = "-1"; };
            act = { priority = "10"; };
            more = { priority = "-1"; alignment = "right"; };
            barend = { priority = "100"; alignment = "right"; };
          };
        };

        window_inact = {

          type = "window";
          placement = "bottom";
          position = "1";
          visible = "inactive";

          items = {
            barstart = { priority = "100"; };
            window = { };
            window_empty = { };
            more = { priority = "-1"; alignment = "right"; };
            barend = { priority = "100"; alignment = "right"; };
          };
        };

        prompt = {

          type = "root";
          placement = "bottom";
          position = "100";
          visible = "always";

          items = {
            prompt = { priority = "-1"; };
            prompt_empty = { priority = "-1"; };
            input = { priority = "10"; };
          };
        };

        topic = {

          type = "root";
          placement = "top";
          position = "1";
          visible = "always";

          items = {
            topicbarstart = { priority = "100"; };
            topic = { };
            topic_empty = { };
            topicbarend = { priority = "100"; alignment = "right"; };
          };
        };
      };
    };
    settings = {
      core = { real_name = "Unknown"; user_name = "nequi"; nick = "nequi"; };
      "fe-text" = { actlist_sort = "refnum"; };
    };
    logs = { };
    windows = { 1 = { immortal = "yes"; name = "(status)"; level = "ALL"; }; };
    mainwindows = { 1 = { first_line = "1"; lines = "53"; }; };
  '';
}
