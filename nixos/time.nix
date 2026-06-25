{
    networking.timeServers = [
      "0.ca.pool.ntp.org"
	  "1.ca.pool.ntp.org"
	  "2.ca.pool.ntp.org"
	  "3.ca.pool.ntp.org"
    ];

    services.ntpd-rs.enable = true;

    time.timeZone = "America/Toronto";
}