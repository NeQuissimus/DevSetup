#!/bin/sh

BASEDIR="$(pwd -P $(dirname $0))"
USERJS="${BASEDIR}/_home/mozilla/user.js"

curl -o "${USERJS}" https://raw.githubusercontent.com/pyllyukko/user.js/master/user.js

# Clean up whitespace
sed -i 's/\s\+/ /g' "${USERJS}"

# Fix a few things that break websites or my usage habits :)
sed -i 's|user_pref("browser.search.update", false);|user_pref("browser.search.update", true);|' "${USERJS}"
sed -i 's|user_pref("privacy.clearOnShutdown.cookies", true);|user_pref("privacy.clearOnShutdown.cookies", false);|' "${USERJS}"
sed -i 's|user_pref("privacy.clearOnShutdown.history", true);|user_pref("privacy.clearOnShutdown.history", false);|' "${USERJS}"
sed -i 's|user_pref("privacy.clearOnShutdown.sessions", true);|user_pref("privacy.clearOnShutdown.sessions", false);|' "${USERJS}"
sed -i 's|user_pref("network.cookie.lifetimePolicy", 2);|user_pref("network.cookie.lifetimePolicy", 0);|' "${USERJS}"
sed -i 's|user_pref("browser.newtabpage.enabled", false);|user_pref("browser.newtabpage.enabled", true);|' "${USERJS}"
sed -i 's|user_pref("browser.newtab.url", "about:blank");||' "${USERJS}"
sed -i 's|user_pref("browser.newtabpage.enhanced", false);|user_pref("browser.newtabpage.enhanced", true);|' "${USERJS}"
sed -i 's|user_pref("browser.newtab.preload", false)|user_pref("browser.newtab.preload", true);|' "${USERJS}"
sed -i 's|user_pref("browser.newtabpage.directory.ping", "");||' "${USERJS}"
sed -i 's|user_pref("browser.newtabpage.directory.source", "data:text/plain,{}");||' "${USERJS}"
sed -i 's|user_pref("browser.urlbar.autocomplete.enabled", false);|user_pref("browser.urlbar.autocomplete.enabled", true);|' "${USERJS}"
sed -i 's|user_pref("browser.urlbar.autoFill", false);|user_pref("browser.urlbar.autoFill", true);|' "${USERJS}"
sed -i 's|user_pref("browser.urlbar.autoFill.typed", false);|user_pref("browser.urlbar.autoFill.typed", true);|' "${USERJS}"
sed -i 's|user_pref("browser.search.suggest.enabled", false);|user_pref("browser.search.suggest.enabled", true);|' "${USERJS}"
sed -i 's|user_pref("browser.urlbar.suggest.searches", false);|user_pref("browser.urlbar.suggest.searches", true);|' "${USERJS}"
sed -i 's|user_pref("browser.urlbar.suggest.history", false);|user_pref("browser.urlbar.suggest.history", true);|' "${USERJS}"
sed -i 's|user_pref("browser.search.update", false);|user_pref("browser.search.update", true);|' "${USERJS}"
sed -i 's|user_pref("browser.pagethumbnails.capturing_disabled", true);|user_pref("browser.pagethumbnails.capturing_disabled", false);|' "${USERJS}"
sed -i 's|user_pref("browser.privatebrowsing.autostart", true);|user_pref("browser.privatebrowsing.autostart", false);|' "${USERJS}"
sed -i 's|user_pref("privacy.clearOnShutdown.openWindows", true);|user_pref("privacy.clearOnShutdown.openWindows", false);|' "${USERJS}"
sed -i 's|user_pref("places.history.enabled", false);|user_pref("places.history.enabled", true);|' "${USERJS}"
sed -i 's|user_pref("privacy.sanitize.sanitizeOnShutdown", true);|user_pref("privacy.sanitize.sanitizeOnShutdown", false);|' "${USERJS}"
sed -i 's|user_pref("svg.disabled", true);|user_pref("svg.disabled", false);|' "${USERJS}"
sed -i 's|user_pref("browser.pocket.enabled", false);|user_pref("browser.pocket.enabled", true);|' "${USERJS}"
sed -i 's|user_pref("extensions.pocket.enabled", false);|user_pref("extensions.pocket.enabled", true);|' "${USERJS}"
sed -i 's|user_pref("keyword.enabled", false);|user_pref("keyword.enabled", true);|' "${USERJS}"
sed -i 's|user_pref("security.fileuri.strict_origin_policy", true);|user_pref("security.fileuri.strict_origin_policy", false);|' "${USERJS}" # Breaks Office OWA
sed -i 's|user_pref("pdfjs.disabled", true);|user_pref("pdfjs.disabled", false);|' "${USERJS}" # View PDFs in browser

echo 'user_pref("zoom.maxPercent", 100);' >> "${USERJS}"
echo 'user_pref("zoom.minPercent", 100);' >> "${USERJS}"

# Remove most comments
sed -i 's|^//.*||g' "${USERJS}"

# Delete empty lines
sed -i '/^$/d' "${USERJS}"

# Add comment in first line
sed -i '1s|^|// First line must be a comment\n|' "${USERJS}"

echo "Now issue the following:"
echo "find ~/.mozilla/firefox -maxdepth 1 -mindepth 1 -type d -exec cp -f ${USERJS} {}/user.js \; -exec rm {}/prefs.js \;"
