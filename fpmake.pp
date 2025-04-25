{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for bgracontrols 9.0.1.6

   This file was generated on 4/14/2025
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_bgracontrols(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('bgracontrols');
    P.Version:='9.0.1-6';

    P.Directory:=ADirectory;

    P.Author:='Dibo, Circular, Lainz and others';
    P.License:='Modified LGPL';
    P.Description:='BGRA Controls is a set of graphical UI elements that you can use with Lazarus LCL applications.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('bgrabitmappack');
    D := P.Dependencies.Add('ideintf');
    P.Options.Add('-MDelphi');
    P.Options.Add('-OoREGVAR');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-gh');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('mouseandkeyinput');
    P.UnitPath.Add('bgrasvgimagelistform');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('bgracontrols.pas');
    t.Dependencies.AddUnit('atshapelinebgra');
    t.Dependencies.AddUnit('bcbasectrls');
    t.Dependencies.AddUnit('bcbrightandcontrast');
    t.Dependencies.AddUnit('bcbutton');
    t.Dependencies.AddUnit('bcbuttonfocus');
    t.Dependencies.AddUnit('bccheckcombobox');
    t.Dependencies.AddUnit('bccombobox');
    t.Dependencies.AddUnit('bcdefaultthememanager');
    t.Dependencies.AddUnit('bceffect');
    t.Dependencies.AddUnit('BCExpandPanels');
    t.Dependencies.AddUnit('bcfilters');
    t.Dependencies.AddUnit('bcfluentprogressring');
    t.Dependencies.AddUnit('bcfluentslider');
    t.Dependencies.AddUnit('bcgamegrid');
    t.Dependencies.AddUnit('bcgradientbutton');
    t.Dependencies.AddUnit('bcimagebutton');
    t.Dependencies.AddUnit('bckeyboard');
    t.Dependencies.AddUnit('bclabel');
    t.Dependencies.AddUnit('bclistbox');
    t.Dependencies.AddUnit('bclistboxex');
    t.Dependencies.AddUnit('bcmaterialdesignbutton');
    t.Dependencies.AddUnit('bcmaterialedit');
    t.Dependencies.AddUnit('bcmaterialfloatspinedit');
    t.Dependencies.AddUnit('bcmaterialprogressbarmarquee');
    t.Dependencies.AddUnit('bcmaterialspinedit');
    t.Dependencies.AddUnit('bcmdbutton');
    t.Dependencies.AddUnit('bcmdbuttonfocus');
    t.Dependencies.AddUnit('bcnumerickeyboard');
    t.Dependencies.AddUnit('bcpanel');
    t.Dependencies.AddUnit('bcradialprogressbar');
    t.Dependencies.AddUnit('bcroundedimage');
    t.Dependencies.AddUnit('bcrtti');
    t.Dependencies.AddUnit('bcsamples');
    t.Dependencies.AddUnit('bcstylesform');
    t.Dependencies.AddUnit('bcsvgbutton');
    t.Dependencies.AddUnit('bcsvgviewer');
    t.Dependencies.AddUnit('bcthememanager');
    t.Dependencies.AddUnit('bctoolbar');
    t.Dependencies.AddUnit('bctools');
    t.Dependencies.AddUnit('bctrackbarupdown');
    t.Dependencies.AddUnit('bctypes');
    t.Dependencies.AddUnit('bgracolortheme');
    t.Dependencies.AddUnit('bgracontrolsinfo');
    t.Dependencies.AddUnit('bgracustomdrawn');
    t.Dependencies.AddUnit('bgraflashprogressbar');
    t.Dependencies.AddUnit('bgragraphiccontrol');
    t.Dependencies.AddUnit('bgraimagelist');
    t.Dependencies.AddUnit('bgraimagemanipulation');
    t.Dependencies.AddUnit('bgraimagetheme');
    t.Dependencies.AddUnit('bgraknob');
    t.Dependencies.AddUnit('bgraresizespeedbutton');
    t.Dependencies.AddUnit('bgrashape');
    t.Dependencies.AddUnit('bgraspeedbutton');
    t.Dependencies.AddUnit('bgraspriteanimation');
    t.Dependencies.AddUnit('bgrasvgimagelist');
    t.Dependencies.AddUnit('bgrasvgtheme');
    t.Dependencies.AddUnit('bgratheme');
    t.Dependencies.AddUnit('bgrathemebutton');
    t.Dependencies.AddUnit('bgrathemecheckbox');
    t.Dependencies.AddUnit('bgrathemeradiobutton');
    t.Dependencies.AddUnit('bgravirtualscreen');
    t.Dependencies.AddUnit('colorspeedbutton');
    t.Dependencies.AddUnit('dtanalogclock');
    t.Dependencies.AddUnit('dtanalogcommon');
    t.Dependencies.AddUnit('dtanaloggauge');
    t.Dependencies.AddUnit('dtthemedclock');
    t.Dependencies.AddUnit('dtthemedgauge');
    t.Dependencies.AddUnit('materialcolors');
    t.Dependencies.AddUnit('bgrasvgimagelistform');
    t.Dependencies.AddUnit('bclealcddisplay');
    t.Dependencies.AddUnit('bclealed');
    t.Dependencies.AddUnit('bcleaqled');
    t.Dependencies.AddUnit('bclearingslider');
    t.Dependencies.AddUnit('bcleaselector');
    t.Dependencies.AddUnit('bcleatheme');
    t.Dependencies.AddUnit('bclealcddisplay_editorregister');
    t.Dependencies.AddUnit('bcleaboard');
    t.Dependencies.AddUnit('bcleaengrave');
    t.Dependencies.AddUnit('supergauge');
    t.Dependencies.AddUnit('supergaugecommon');
    t.Dependencies.AddUnit('bgradialogs');
    t.Dependencies.AddUnit('superspinnercommon');
    t.Dependencies.AddUnit('superspinner');

    T:=P.Targets.AddUnit('atshapelinebgra.pas');
    P.Targets.AddImplicitUnit('bcbasectrls.pas');
    P.Targets.AddImplicitUnit('bcbrightandcontrast.pas');
    T:=P.Targets.AddUnit('bcbutton.pas');
    T:=P.Targets.AddUnit('bcbuttonfocus.pas');
    T:=P.Targets.AddUnit('bccheckcombobox.pas');
    T:=P.Targets.AddUnit('bccombobox.pas');
    P.Targets.AddImplicitUnit('bcdefaultthememanager.pas');
    T:=P.Targets.AddUnit('bceffect.pas');
    T:=P.Targets.AddUnit('BCExpandPanels.pas');
    T:=P.Targets.AddUnit('bcfilters.pas');
    T:=P.Targets.AddUnit('bcfluentprogressring.pas');
    T:=P.Targets.AddUnit('bcfluentslider.pas');
    T:=P.Targets.AddUnit('bcgamegrid.pas');
    T:=P.Targets.AddUnit('bcgradientbutton.pas');
    T:=P.Targets.AddUnit('bcimagebutton.pas');
    P.Targets.AddImplicitUnit('bckeyboard.pas');
    T:=P.Targets.AddUnit('bclabel.pas');
    T:=P.Targets.AddUnit('bclistbox.pas');
    T:=P.Targets.AddUnit('bclistboxex.pas');
    T:=P.Targets.AddUnit('bcmaterialdesignbutton.pas');
    T:=P.Targets.AddUnit('bcmaterialedit.pas');
    T:=P.Targets.AddUnit('bcmaterialfloatspinedit.pas');
    T:=P.Targets.AddUnit('bcmaterialprogressbarmarquee.pas');
    T:=P.Targets.AddUnit('bcmaterialspinedit.pas');
    T:=P.Targets.AddUnit('bcmdbutton.pas');
    T:=P.Targets.AddUnit('bcmdbuttonfocus.pas');
    P.Targets.AddImplicitUnit('bcnumerickeyboard.pas');
    T:=P.Targets.AddUnit('bcpanel.pas');
    T:=P.Targets.AddUnit('bcradialprogressbar.pas');
    T:=P.Targets.AddUnit('bcroundedimage.pas');
    T:=P.Targets.AddUnit('bcrtti.pas');
    T:=P.Targets.AddUnit('bcsamples.pas');
    T:=P.Targets.AddUnit('bcstylesform.pas');
    T:=P.Targets.AddUnit('bcsvgbutton.pas');
    T:=P.Targets.AddUnit('bcsvgviewer.pas');
    P.Targets.AddImplicitUnit('bcthememanager.pas');
    T:=P.Targets.AddUnit('bctoolbar.pas');
    P.Targets.AddImplicitUnit('bctools.pas');
    T:=P.Targets.AddUnit('bctrackbarupdown.pas');
    P.Targets.AddImplicitUnit('bctypes.pas');
    T:=P.Targets.AddUnit('bgracolortheme.pas');
    T:=P.Targets.AddUnit('bgracontrolsinfo.pas');
    T:=P.Targets.AddUnit('bgracustomdrawn.pas');
    T:=P.Targets.AddUnit('bgraflashprogressbar.pas');
    T:=P.Targets.AddUnit('bgragraphiccontrol.pas');
    T:=P.Targets.AddUnit('bgraimagelist.pas');
    T:=P.Targets.AddUnit('bgraimagemanipulation.pas');
    T:=P.Targets.AddUnit('bgraimagetheme.pas');
    T:=P.Targets.AddUnit('bgraknob.pas');
    T:=P.Targets.AddUnit('bgraresizespeedbutton.pas');
    T:=P.Targets.AddUnit('bgrashape.pas');
    T:=P.Targets.AddUnit('bgraspeedbutton.pas');
    T:=P.Targets.AddUnit('bgraspriteanimation.pas');
    T:=P.Targets.AddUnit('bgrasvgimagelist.pas');
    T:=P.Targets.AddUnit('bgrasvgtheme.pas');
    T:=P.Targets.AddUnit('bgratheme.pas');
    T:=P.Targets.AddUnit('bgrathemebutton.pas');
    T:=P.Targets.AddUnit('bgrathemecheckbox.pas');
    P.Targets.AddImplicitUnit('bgrathemeradiobutton.pas');
    T:=P.Targets.AddUnit('bgravirtualscreen.pas');
    T:=P.Targets.AddUnit('colorspeedbutton.pas');
    T:=P.Targets.AddUnit('dtanalogclock.pas');
    T:=P.Targets.AddUnit('dtanalogcommon.pas');
    P.Targets.AddImplicitUnit('dtanaloggauge.pas');
    T:=P.Targets.AddUnit('dtthemedclock.pas');
    T:=P.Targets.AddUnit('dtthemedgauge.pas');
    T:=P.Targets.AddUnit('materialcolors.pas');
    T:=P.Targets.AddUnit('bgrasvgimagelistform\bgrasvgimagelistform.pas');
    T:=P.Targets.AddUnit('bclealcddisplay.pas');
    T:=P.Targets.AddUnit('bclealed.pas');
    T:=P.Targets.AddUnit('bcleaqled.pas');
    T:=P.Targets.AddUnit('bclearingslider.pas');
    T:=P.Targets.AddUnit('bcleaselector.pas');
    T:=P.Targets.AddUnit('bcleatheme.pas');
    T:=P.Targets.AddUnit('bclealcddisplay_editorregister.pas');
    T:=P.Targets.AddUnit('bcleaboard.pas');
    T:=P.Targets.AddUnit('bcleaengrave.pas');
    T:=P.Targets.AddUnit('supergauge.pas');
    T:=P.Targets.AddUnit('supergaugecommon.pas');
    T:=P.Targets.AddUnit('bgradialogs.pas');
    T:=P.Targets.AddUnit('superspinnercommon.pas');
    T:=P.Targets.AddUnit('superspinner.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('bgracontrols.compiled');
    P.InstallFiles.Add('bgracontrols.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_bgracontrols('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
