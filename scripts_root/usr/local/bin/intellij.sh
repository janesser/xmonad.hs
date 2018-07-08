STARTER=`which intellij-idea-community`

## JAVA_HOME
# override with bundled when present
BUNDLED_JRE=/opt/intellij-idea-community/jre64/
if [ -f $BUNDLED_JRE ]; then
  export JAVA_HOME=$BUNDLED_JRE
  export PATH=/opt/intellij-idea-community/jre64/bin:$PATH
fi

## DETECT & RUN STARTER
# gtk2-mode & xmonad tweak
if [ -f $STARTER ]; then
  _JAVA_AWT_WM_NONREPARENTING=1 SWT_GTK3=0 $STARTER $*
fi
