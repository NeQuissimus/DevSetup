// First line must be a comment
/******************************************************************************
 * user.js *
 * https://github.com/pyllyukko/user.js *
 ******************************************************************************/
/******************************************************************************
 * SECTION: HTML5 / APIs / DOM *
 ******************************************************************************/
user_pref("dom.serviceWorkers.enabled", false);
user_pref("dom.workers.enabled", false);
user_pref("dom.webnotifications.enabled", false);
user_pref("dom.enable_performance", false);
user_pref("dom.enable_user_timing", false);
user_pref("dom.webaudio.enabled", false);
user_pref("geo.enabled", false);
user_pref("geo.wifi.uri", "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%");
user_pref("geo.wifi.logging.enabled", false);
user_pref("dom.mozTCPSocket.enabled", false);
user_pref("dom.netinfo.enabled", false);
user_pref("media.peerconnection.enabled", false);
user_pref("media.peerconnection.ice.default_address_only", true); // Firefox 42-51
user_pref("media.peerconnection.ice.no_host", true); // Firefox >= 52
user_pref("media.navigator.enabled", false);
user_pref("media.navigator.video.enabled", false);
user_pref("media.getusermedia.screensharing.enabled", false);
user_pref("media.getusermedia.audiocapture.enabled", false);
user_pref("dom.battery.enabled", false);
user_pref("dom.telephony.enabled", false);
user_pref("beacon.enabled", false);
user_pref("dom.event.clipboardevents.enabled", false);
user_pref("dom.allow_cut_copy", false);
user_pref("media.webspeech.recognition.enable", false);
user_pref("media.webspeech.synth.enabled", false);
user_pref("device.sensors.enabled", false);
user_pref("browser.send_pings", false);
user_pref("browser.send_pings.require_same_host", true);
user_pref("dom.gamepad.enabled", false);
user_pref("dom.vr.enabled", false);
user_pref("dom.vibrator.enabled", false);
user_pref("dom.enable_resource_timing", false);
user_pref("dom.archivereader.enabled", false);
user_pref("webgl.disabled", true);
user_pref("webgl.min_capability_mode", true);
user_pref("webgl.disable-extensions", true);
user_pref("webgl.disable-fail-if-major-performance-caveat", true);
user_pref("webgl.enable-debug-renderer-info", false);
/******************************************************************************
 * SECTION: Misc *
 ******************************************************************************/
user_pref("camera.control.face_detection.enabled", false);
user_pref("browser.search.countryCode", "US");
user_pref("browser.search.region", "US");
user_pref("browser.search.geoip.url", "");
user_pref("intl.accept_languages", "en-us, en");
user_pref("general.useragent.locale", "en-US");
user_pref("intl.locale.matchOS", false);
user_pref("browser.search.geoSpecificDefaults", false);
user_pref("clipboard.autocopy", false);
user_pref("javascript.use_us_english_locale", true);
user_pref("keyword.enabled", true);
user_pref("browser.urlbar.trimURLs", false);
user_pref("browser.fixup.alternate.enabled", false);
user_pref("browser.fixup.hide_user_pass", true);
user_pref("network.proxy.socks_remote_dns", true);
user_pref("network.manage-offline-status", false);
user_pref("security.mixed_content.block_active_content", true);
user_pref("security.mixed_content.block_display_content", true);
user_pref("network.jar.open-unsafe-types", false);
user_pref("security.xpconnect.plugin.unrestricted", false);
user_pref("security.fileuri.strict_origin_policy", false);
user_pref("browser.urlbar.filter.javascript", true);
user_pref("javascript.options.asmjs", false);
user_pref("gfx.font_rendering.opentype_svg.enabled", false);
user_pref("svg.disabled", false);
user_pref("media.video_stats.enabled", false);
user_pref("general.buildID.override", "20100101");
user_pref("browser.display.use_document_fonts", 0);
user_pref("network.protocol-handler.warn-external-default", true);
user_pref("network.protocol-handler.external.http", false);
user_pref("network.protocol-handler.external.https", false);
user_pref("network.protocol-handler.external.javascript", false);
user_pref("network.protocol-handler.external.moz-extension", false);
user_pref("network.protocol-handler.external.ftp", false);
user_pref("network.protocol-handler.external.file", false);
user_pref("network.protocol-handler.external.about", false);
user_pref("network.protocol-handler.external.chrome", false);
user_pref("network.protocol-handler.external.blob", false);
user_pref("network.protocol-handler.external.data", false);
user_pref("network.protocol-handler.expose-all", false);
user_pref("network.protocol-handler.expose.http", true);
user_pref("network.protocol-handler.expose.https", true);
user_pref("network.protocol-handler.expose.javascript", true);
user_pref("network.protocol-handler.expose.moz-extension", true);
user_pref("network.protocol-handler.expose.ftp", true);
user_pref("network.protocol-handler.expose.file", true);
user_pref("network.protocol-handler.expose.about", true);
user_pref("network.protocol-handler.expose.chrome", true);
user_pref("network.protocol-handler.expose.blob", true);
user_pref("network.protocol-handler.expose.data", true);
/******************************************************************************
 * SECTION: Extensions / plugins *
 ******************************************************************************/
user_pref("security.dialog_enable_delay", 1000);
user_pref("extensions.getAddons.cache.enabled", false);
user_pref("lightweightThemes.update.enabled", false);
user_pref("plugin.state.flash", 0);
user_pref("plugin.state.java", 0);
user_pref("dom.ipc.plugins.flash.subprocess.crashreporter.enabled", false);
user_pref("dom.ipc.plugins.reportCrashURL", false);
user_pref("browser.safebrowsing.blockedURIs.enabled", true);
pref("shumway.disabled", true);
user_pref("plugin.state.libgnome-shell-browser-plugin", 0);
user_pref("plugins.click_to_play", true);
user_pref("extensions.update.enabled", true);
user_pref("extensions.blocklist.enabled", true);
user_pref("services.blocklist.update_enabled", true);
user_pref("extensions.blocklist.url", "https://blocklist.addons.mozilla.org/blocklist/3/%APP_ID%/%APP_VERSION%/");
/******************************************************************************
 * SECTION: Firefox (anti-)features / components * *
 ******************************************************************************/
user_pref("devtools.webide.enabled", false);
user_pref("devtools.webide.autoinstallADBHelper", false);
user_pref("devtools.webide.autoinstallFxdtAdapters", false);
user_pref("devtools.debugger.remote-enabled", false);
user_pref("devtools.chrome.enabled", false);
user_pref("devtools.debugger.force-local", true);
user_pref("toolkit.telemetry.enabled", false);
user_pref("toolkit.telemetry.unified", false);
user_pref("experiments.supported", false);
user_pref("experiments.enabled", false);
user_pref("experiments.manifest.uri", "");
user_pref("network.allow-experiments", false);
user_pref("breakpad.reportURL", "");
user_pref("browser.tabs.crashReporting.sendReport", false);
user_pref("browser.crashReports.unsubmittedCheck.enabled", false);
user_pref("dom.flyweb.enabled", false);
user_pref("browser.uitour.enabled", false);
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.pbmode.enabled", true);
user_pref("privacy.userContext.enabled", true);
user_pref("privacy.resistFingerprinting", true);
user_pref("pdfjs.disabled", true);
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.healthreport.service.enabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("browser.selfsupport.url", "");
user_pref("loop.logDomains", false);
user_pref("app.update.enabled", true);
user_pref("browser.safebrowsing.enabled", true); // Firefox < 50
user_pref("browser.safebrowsing.phishing.enabled", true); // firefox >= 50
user_pref("browser.safebrowsing.malware.enabled", true);
user_pref("browser.safebrowsing.downloads.remote.enabled", false);
user_pref("browser.pocket.enabled", true);
user_pref("extensions.pocket.enabled", true);
/******************************************************************************
 * SECTION: Automatic connections *
 ******************************************************************************/
user_pref("network.prefetch-next", false);
user_pref("network.dns.disablePrefetch", true);
user_pref("network.dns.disablePrefetchFromHTTPS", true);
user_pref("network.predictor.enabled", false);
user_pref("network.dns.blockDotOnion", true);
user_pref("browser.search.suggest.enabled", true);
user_pref("browser.urlbar.suggest.searches", true);
user_pref("browser.urlbar.suggest.history", true);
user_pref("browser.casting.enabled", false);
user_pref("media.gmp-gmpopenh264.enabled", false);
user_pref("media.gmp-manager.url", "");
user_pref("network.http.speculative-parallel-limit", 0);
user_pref("browser.aboutHomeSnippets.updateUrl", "");
user_pref("browser.search.update", true);
/******************************************************************************
 * SECTION: HTTP *
 ******************************************************************************/
user_pref("network.negotiate-auth.allow-insecure-ntlm-v1", false);
user_pref("security.csp.experimentalEnabled", true);
user_pref("security.csp.enable", true);
user_pref("security.sri.enable", true);
user_pref("network.http.referer.spoofSource", true);
user_pref("network.cookie.cookieBehavior", 1);
user_pref("network.cookie.thirdparty.sessionOnly", true);
/*******************************************************************************
 * SECTION: Caching *
 ******************************************************************************/
user_pref("browser.privatebrowsing.autostart", false);
user_pref("browser.cache.offline.enable", false);
user_pref("privacy.sanitize.sanitizeOnShutdown", false);
user_pref("privacy.clearOnShutdown.cache", true);
user_pref("privacy.clearOnShutdown.cookies", false);
user_pref("privacy.clearOnShutdown.downloads", true);
user_pref("privacy.clearOnShutdown.formdata", true);
user_pref("privacy.clearOnShutdown.history", false);
user_pref("privacy.clearOnShutdown.offlineApps", true);
user_pref("privacy.clearOnShutdown.sessions", false);
user_pref("privacy.clearOnShutdown.openWindows", false);
user_pref("privacy.sanitize.timeSpan", 0);
user_pref("privacy.cpd.offlineApps", true);
user_pref("privacy.cpd.cache", true);
user_pref("privacy.cpd.cookies", true);
user_pref("privacy.cpd.downloads", true);
user_pref("privacy.cpd.formdata", true);
user_pref("privacy.cpd.history", true);
user_pref("privacy.cpd.sessions", true);
user_pref("places.history.enabled", true);
user_pref("browser.cache.disk.enable", false);
user_pref("browser.cache.disk_cache_ssl", false);
user_pref("browser.download.manager.retention", 0);
user_pref("signon.rememberSignons", false);
user_pref("browser.formfill.enable", false);
user_pref("network.cookie.lifetimePolicy", 0);
user_pref("signon.autofillForms", false);
user_pref("signon.autofillForms.http", false);
user_pref("security.insecure_field_warning.contextual.enabled", true);
user_pref("browser.formfill.expire_days", 0);
user_pref("browser.sessionstore.privacy_level", 2);
user_pref("browser.helperApps.deleteTempFileOnExit", true);
user_pref("browser.pagethumbnails.capturing_disabled", false);
user_pref("browser.shell.shortcutFavicons", false);
user_pref("browser.bookmarks.max_backups", 0);
/*******************************************************************************
 * SECTION: UI related *
 *******************************************************************************/
user_pref("security.insecure_password.ui.enabled", true);
user_pref("browser.download.folderList", 2);
user_pref("browser.download.useDownloadDir", false);
user_pref("browser.newtabpage.enabled", true);
user_pref("browser.newtabpage.enhanced", true);
user_pref("browser.newtab.preload", true);;
user_pref("plugins.update.notifyUser", true);
user_pref("network.IDN_show_punycode", true);
user_pref("browser.urlbar.autoFill", true);
user_pref("browser.urlbar.autoFill.typed", true);
user_pref("layout.css.visited_links_enabled", false);
user_pref("browser.urlbar.autocomplete.enabled", true);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("security.ask_for_password", 2);
user_pref("security.password_lifetime", 1);
user_pref("browser.offline-apps.notify", true);
/******************************************************************************
 * SECTION: Cryptography *
 ******************************************************************************/
user_pref("network.stricttransportsecurity.preloadlist", true);
user_pref("security.OCSP.enabled", 1);
user_pref("security.ssl.enable_ocsp_stapling", true);
user_pref("security.ssl.enable_ocsp_must_staple", true);
user_pref("security.OCSP.require", true);
user_pref("security.ssl.disable_session_identifiers", true);
user_pref("security.tls.version.min", 1);
user_pref("security.tls.version.max", 4);
user_pref("security.tls.version.fallback-limit", 3);
user_pref("security.cert_pinning.enforcement_level", 2);
user_pref("security.pki.sha1_enforcement_level", 1);
user_pref("security.ssl.treat_unsafe_negotiation_as_broken", true);
user_pref("security.ssl.errorReporting.automatic", false);
user_pref("browser.ssl_override_behavior", 1);
/******************************************************************************
 * SECTION: Cipher suites *
 ******************************************************************************/
user_pref("security.ssl3.rsa_null_sha", false);
user_pref("security.ssl3.rsa_null_md5", false);
user_pref("security.ssl3.ecdhe_rsa_null_sha", false);
user_pref("security.ssl3.ecdhe_ecdsa_null_sha", false);
user_pref("security.ssl3.ecdh_rsa_null_sha", false);
user_pref("security.ssl3.ecdh_ecdsa_null_sha", false);
user_pref("security.ssl3.rsa_seed_sha", false);
user_pref("security.ssl3.rsa_rc4_40_md5", false);
user_pref("security.ssl3.rsa_rc2_40_md5", false);
user_pref("security.ssl3.rsa_1024_rc4_56_sha", false);
user_pref("security.ssl3.rsa_camellia_128_sha", false);
user_pref("security.ssl3.ecdhe_rsa_aes_128_sha", false);
user_pref("security.ssl3.ecdhe_ecdsa_aes_128_sha", false);
user_pref("security.ssl3.ecdh_rsa_aes_128_sha", false);
user_pref("security.ssl3.ecdh_ecdsa_aes_128_sha", false);
user_pref("security.ssl3.dhe_rsa_camellia_128_sha", false);
user_pref("security.ssl3.dhe_rsa_aes_128_sha", false);
user_pref("security.ssl3.ecdh_ecdsa_rc4_128_sha", false);
user_pref("security.ssl3.ecdh_rsa_rc4_128_sha", false);
user_pref("security.ssl3.ecdhe_ecdsa_rc4_128_sha", false);
user_pref("security.ssl3.ecdhe_rsa_rc4_128_sha", false);
user_pref("security.ssl3.rsa_rc4_128_md5", false);
user_pref("security.ssl3.rsa_rc4_128_sha", false);
user_pref("security.tls.unrestricted_rc4_fallback", false);
user_pref("security.ssl3.dhe_dss_des_ede3_sha", false);
user_pref("security.ssl3.dhe_rsa_des_ede3_sha", false);
user_pref("security.ssl3.ecdh_ecdsa_des_ede3_sha", false);
user_pref("security.ssl3.ecdh_rsa_des_ede3_sha", false);
user_pref("security.ssl3.ecdhe_ecdsa_des_ede3_sha", false);
user_pref("security.ssl3.ecdhe_rsa_des_ede3_sha", false);
user_pref("security.ssl3.rsa_des_ede3_sha", false);
user_pref("security.ssl3.rsa_fips_des_ede3_sha", false);
user_pref("security.ssl3.ecdh_rsa_aes_256_sha", false);
user_pref("security.ssl3.ecdh_ecdsa_aes_256_sha", false);
user_pref("security.ssl3.rsa_camellia_256_sha", false);
user_pref("security.ssl3.ecdhe_rsa_aes_256_sha", true); // 0xc014
user_pref("security.ssl3.ecdhe_ecdsa_aes_256_sha", true); // 0xc00a
user_pref("security.ssl3.ecdhe_ecdsa_aes_128_gcm_sha256", true); // 0xc02b
user_pref("security.ssl3.ecdhe_rsa_aes_128_gcm_sha256", true); // 0xc02f
user_pref("security.ssl3.ecdhe_ecdsa_chacha20_poly1305_sha256", true);
user_pref("security.ssl3.ecdhe_rsa_chacha20_poly1305_sha256", true);
user_pref("security.ssl3.dhe_rsa_camellia_256_sha", false);
user_pref("security.ssl3.dhe_rsa_aes_256_sha", false);
user_pref("security.ssl3.dhe_dss_aes_128_sha", false);
user_pref("security.ssl3.dhe_dss_aes_256_sha", false);
user_pref("security.ssl3.dhe_dss_camellia_128_sha", false);
user_pref("security.ssl3.dhe_dss_camellia_256_sha", false);
user_pref("security.ssl3.rsa_aes_256_sha", true); // 0x35
user_pref("security.ssl3.rsa_aes_128_sha", true); // 0x2f
