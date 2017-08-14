# System configuration - Basics

Now that we have NixOS installed, we'd like to actually be productive.
We would like to have some sort of desktop environment, applications, users, etc.
These are the basics we require to be productive.

Note that whenever we have a set of Nix configurations in this tutorial, they generally belong into `/etc/nixos/configuration.nix` and require either `nixos-rebuild switch` or `nixos-rebuild boot` followed by a reboot (I would recommend the reboot method).

# X Server

Let's start with a graphical desktop environment. Something on top of X Server would be nice.
A variety of options are available under `services.xserver`. To start, we are going to turn on X and select a display manager.
We will also need a window manager, so here we go:

```nix
services.xserver = {
  autorun = true;
  enable = true;
  displayManager.lightdm.enable = true;
  windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
};
```

Here, we chose LightDM as our display manager and XMonad to handle our windows. Note that there are plenty of alternatives available with NixOS.
From GNOME to KDE to i3, there should be a window manager for everyone.

That's it. Upon reboot, we are asked to log in via LightDM. However, at this moment we only have `root` to log in with.
We need a user account, so we can start relying on `root` a little less and give up those system privileges that may hurt us.

# Users

```nix
users.extraUsers.alice = {
  extraGroups = [ "wheel" ];
  isNormalUser = true;
  name = "alice";
  uid = 1000;
};
```

Easy, right? Create an additional user `alice`, add them to the `wheel` group for `sudo` access and mark them as a "normal user".
In this case, normal means that the user will have `/home/<alias>` and not be a system user.
NixOS simply provides a shortcut for these rather typical settings. It is, of course, possible to change these settings yourself.
Consult the [NixOS Manual](https://nixos.org/nixos/manual/) for more information.

Once the user(s) have been created, `root` will need to provide them with a password. (Or `su` in through `root` and set the password that way)
After that, our new user is able to log into LightDM and access the graphical desktop environment.

Now that we can log in as a non-root user, we are able to be productive. Or almost at least, let's add some essential applications!

# Applications

There are two major ways to install packages/applications on NixOS: System-wide or into the user environment.
For simplicity, we are going to install everything system-wide. Later on, we will discover user environments a little more
and point out advantages of keeping (at least some) packages limited to specific users.

```nix
environment.systemPackages = with pkgs; [
  chromium
  dmenu
  git
  htop
  jq
  skopeo
];
```

Available packages can be discovered using `nix-env -qaP '.*foo.*'`. `P` means pattern and defines the search as a regular expression.
We can then take the names of the packages we have found and list them in our `systemPackages`.
In this example, we are installing `chromium` (web browser), `dmenu` (command launcher), `git` (version control), `htop` (system analysis), `jq` (JSON printer/query) and `skopeo` (Docker inspection). Fill this list with whatever applications are needed.

# Localization

```nix
i18n = {
  consoleKeyMap = "us";
  defaultLocale = "en_CA.UTF-8";
};

time = {
  timeZone = "America/Toronto";
};
```

Here are two blocks of configuration that define where we are and how we would like to type. The values are pretty self-explanatory:
Standard locales and time zone definitions for Linux apply.

# Networking

Networking can be a complex topic, so we are going to assume a best case scenario: Everything works and just needs to be tweaked a little bit.

```nix
networking = {
  firewall = {
    allowPing = false;
    enable = true;
  };

  hostName = "nixus";
}:
```

Enable the firewall, don't allow ICMP pings and set our host name.

Now we are ready to go, have all our essential applications installed onto a beautiful desktop environment and are using a minimal amount of
system resources (just check `htop`!).
