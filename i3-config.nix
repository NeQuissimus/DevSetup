{ config, lib, pkgs, ... }:
rec {
  services.xserver.windowManager.i3 = {
    configFile = "/etc/i3/config";
    enable = true;
  };

  environment.etc."i3/status".text = ''
    general {
      colors = true
      interval = 1
      colors = true
      color_good = "#36E592"
      color_degraded = "#CE9726"
      color_bad = "#CE4B4F"
      color_separator = "#B3BEFF"
      interval = 5
    }

    order += "ipv6"
    order += "ethernet eno1"
    order += "wireless wlp1s0"
    order += "battery 0"
    order += "cpu_usage"
    order += "disk /"
    order += "disk /home"
    order += "time"

    ipv6 {
      format_down = ""
    }

    battery 0 {
      format = " %percentage"
      format_down = ""
    }

    ethernet eno1 {
      format_up = " eno1   %ip"
      format_down = ""
    }

    wireless wlp1s0 {
      format_up = "%essid (%quality): %ip"
      format_down = ""
    }

    time {
      format = " %a, %d %b %H:%M"
    }

    cpu_usage {
      format = " %usage"
    }

    disk "/" {
      format = "  %avail (root)"
    }

    disk "/home" {
      format = "  %avail (home)"
    }
  '';

  environment.etc."i3/config".text = ''
    font pango:Source Code Pro 8

    set $mod Mod4
    set $dmenu_run $(find /nix/store -name 'dmenu_run' -type f | head -1)
    set $i3status_run $(find /nix/store -name 'i3status' -type f | head -1)

    floating_modifier $mod

    bindsym $mod+Return exec xterm
    bindsym $mod+Shift+q kill
    bindsym $mod+d exec $dmenu_run
    bindsym $mod+Left focus left
    bindsym $mod+Down focus down
    bindsym $mod+Up focus up
    bindsym $mod+Right focus right
    bindsym $mod+Shift+Left move left
    bindsym $mod+Shift+Down move down
    bindsym $mod+Shift+Up move up
    bindsym $mod+Shift+Right move right
    bindsym $mod+h split h
    bindsym $mod+v split v
    bindsym $mod+f fullscreen toggle
    bindsym $mod+s layout stacking
    bindsym $mod+w layout tabbed
    bindsym $mod+e layout toggle split
    bindsym $mod+Shift+space floating toggle
    bindsym $mod+space focus mode_toggle
    bindsym $mod+a focus parent

    set $workspace9 "9: "
    set $workspace10 "10: "

    bindsym $mod+1 workspace 1
    bindsym $mod+2 workspace 2
    bindsym $mod+3 workspace 3
    bindsym $mod+4 workspace 4
    bindsym $mod+5 workspace 5
    bindsym $mod+6 workspace 6
    bindsym $mod+7 workspace 7
    bindsym $mod+8 workspace 8
    bindsym $mod+9 workspace $workspace9
    bindsym $mod+0 workspace $workspace10
    bindsym $mod+Shift+1 move container to workspace 1
    bindsym $mod+Shift+2 move container to workspace 2
    bindsym $mod+Shift+3 move container to workspace 3
    bindsym $mod+Shift+4 move container to workspace 4
    bindsym $mod+Shift+5 move container to workspace 5
    bindsym $mod+Shift+6 move container to workspace 6
    bindsym $mod+Shift+7 move container to workspace 7
    bindsym $mod+Shift+8 move container to workspace 8
    bindsym $mod+Shift+9 move container to workspace $workspace9
    bindsym $mod+Shift+0 move container to workspace $workspace10

    assign [class="Firefox"] $workspace9
    assign [class="Slack"] $workspace10
    assign [class="Franz"] $workspace10

    workspace $workspace9 output DP2
    workspace $workspace10 output DP2

    bindsym $mod+bracketleft move workspace to output left
    bindsym $mod+bracketright move workspace to output right

    bindsym $mod+Shift+l exec i3lock-fancy
    bindsym $mod+Shift+c reload
    bindsym $mod+Shift+r restart
    bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

    mode "resize" {
            bindsym Left resize shrink width 10 px or 10 ppt
            bindsym Down resize grow height 10 px or 10 ppt
            bindsym Up resize shrink height 10 px or 10 ppt
            bindsym Right resize grow width 10 px or 10 ppt
            bindsym Return mode "default"
            bindsym Escape mode "default"
    }

    bindsym $mod+r mode "resize"

    set $bg-color            #2f343f
    set $inactive-bg-color   #2f343f
    set $text-color          #f3f4f5
    set $inactive-text-color #676E7D
    set $urgent-bg-color     #E53935
    set $indicator-color     #666666

    client.focused          $bg-color           $bg-color          $text-color          $indicator-color
    client.unfocused        $inactive-bg-color  $inactive-bg-color $inactive-text-color $indicator-color
    client.focused_inactive $inactive-bg-color  $inactive-bg-color $inactive-text-color $indicator-color
    client.urgent           $urgent-bg-color    $urgent-bg-color   $text-color          $indicator-color

    bar {
            status_command ${pkgs.i3status}/bin/i3status -c /etc/i3/status
            colors {
            background $bg-color
            separator #757575

            focused_workspace  $bg-color          $bg-color          $text-color
            inactive_workspace $inactive-bg-color $inactive-bg-color $inactive-text-color
            urgent_workspace   $urgent-bg-color   $urgent-bg-color   $text-color
            }
    }

    exec_always xsetroot -solid black
  '';
}
