{
  environment.shellInit = ''cat << EOF > ~/XTerm
  XTerm*Background: grey10
  XTerm*Foreground: white
  XTerm*FaceName: SourceCode Pro:size=10:antialias=true
  XTerm*Font: 8x13
  XTerm*SaveLines: 10000
  XTerm*BorderWidth: 5
  XTerm*BorderColour: white

  ! tangoesque scheme
  *background: #111111
  *foreground: #babdb6
  ! Black (not tango) + DarkGrey
  *color0:  #000000
  *color8:  #555753
  ! DarkRed + Red
  *color1:  #ff6565
  *color9:  #ff8d8d
  ! DarkGreen + Green
  *color2:  #93d44f
  *color10: #c8e7a8
  ! DarkYellow + Yellow
  *color3:  #eab93d
  *color11: #ffc123
  ! DarkBlue + Blue
  *color4:  #204a87
  *color12: #3465a4
  ! DarkMagenta + Magenta
  *color5:  #ce5c00
  *color13: #f57900
  !DarkCyan + Cyan (both not tango)
  *color6:  #89b6e2
  *color14: #46a4ff
  ! LightGrey + White
  *color7:  #cccccc
  *color15: #ffffff
EOF'';
}
