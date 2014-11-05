(*
  Libray for building html pages

  Copyright (C) 2014 Costas Velissariou
  velissariouc@gmail.com

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit cvAngular;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cvElement;

type
  //... Bootstrap elements ...................

  TViewPort = class;

  { TUIBootstrap }

  TUIBootstrap = class // AngularJS Bootstrap
  public
    doc: THtml; //owner
    css: string;
    BootStrapScript, AngularScript: string;
    Viewport: TViewPort;
    //constructor create( aHead: THead);
    constructor create( aDoc: THtml);
    procedure OnBeforeHtml;
  end;


  { TViewPort }

  TViewPort = class( TMeta)
  protected
    function GetExtraMetaContent: string; override;
  public
    MetaWidth: string;
    InitialScale: string;
    MaximumScale, UserSscalable: string;
    constructor create( aParent: TElement);
  end;

  { TNavMenu }

  TNavMenu = class( TUnorderedList)
    constructor create( aParent: TElement);
    function AddItem( aContent: string; href:string='#'): TNavMenu;

  end;

  { TNavBar }

  TNavBar = class( TElement)
    container, header: TDiv;
    BrandAnchor: TAnchor;
    Brand: string;
    constructor create( aParent: TElement);
    function html: string; override;
    function AddMenu: TNavMenu;
  end;

  { TAngularHtml }

  TAngularHtml = class( THtml)
  public
    Bootstrap: TUIBootstrap;
    constructor create( aTitle: string = '');
end;

TAngular = class
  Doc: THtml;
  name: string;
  AngularScript: string;
  constructor create( aDoc: THtml);
  procedure OnBeforeHtml;
end;


implementation

{ TAngular }

constructor TAngular.create(aDoc: THtml);
begin
  Doc := aDoc;
  name := 'MyApp';
  AngularScript := '/js/angular.min.js';
  Doc.UseLibrary( @OnBeforeHtml);
end;

procedure TAngular.OnBeforeHtml;
begin
  Doc.Attributes.Add('ng-app="'+ name +'"');
  doc.head.SetJScriptSrc('ANGULAR', AngularScript);
  //doc.AddClass();
end;

{ TAngularHtml }

constructor TAngularHtml.create(aTitle: string);
begin
  inherited create( aTitle);
end;

{ TViewPort }

function TViewPort.GetExtraMetaContent: string;
var
  s: string;
begin
  //s := inherited GetExtraAttributes;
  s := '';
  if MetaWidth <> '' then
    s := s + ' width=' + MetaWidth + ',';
  if InitialScale <> '' then
    s := s + ' initial-scale=' + InitialScale + ',';
  if MaximumScale <> '' then
    s := s + ' maximum-scale=' + MaximumScale + ',';
  if UserSscalable <> '' then
    s := s + ' user-scalable=' + UserSscalable + ',';
  if s <> '' then
    Delete( s, length(s), 1);
  result := s;
end;

constructor TViewPort.create(aParent: TElement);
begin
  inherited create(aParent);
  name := 'viewport';
  MetaWidth := 'device-width';
  initialScale:='1';
end;

{ TNavMenu }

constructor TNavMenu.create(aParent: TElement);
begin
 inherited create( aParent, 'nav');
 AddClass('navbar-nav');
end;

function TNavMenu.AddItem(aContent: string; href: string): TNavMenu;
var
 li: TListItem;
 a: TAnchor;
begin
 inherited AddItem('', li);
 a := TAnchor.create( li);
 a.href := href;
 a.addContent( aContent);
 result := self;
end;

{ TNavBar }

constructor TNavBar.create(aParent: TElement);
//var
 //container: TDiv;
begin
 inherited create( aParent);
 tag := 'nav';
 AddClass( 'navbar');
 AddClass( 'navbar-default');

 container := TDiv.create( self, 'container-fluid');

 header := TDiv.create( container, 'navbar-header');

 BrandAnchor := TAnchor.create( header);
 BrandAnchor.AddClass('navbar-brand');
 BrandAnchor.href:='#';
end;

function TNavBar.html: string;
begin
 BrandAnchor.enabled := Brand <> '';
 BrandAnchor.Content.Text := Brand;
 Result:=inherited html;
end;

function TNavBar.AddMenu: TNavMenu;
begin
 result := TNavMenu.create( container);
end;

{ TUIBootstrap }



constructor TUIBootstrap.create(aDoc: THtml);
begin
 Doc := aDoc;
 Viewport := TViewPort.create( Doc.head);

 Css := 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css';
 BootStrapScript := '/js/ui-bootstrap-tpls.min.js';
 AngularScript := '/js/angular.min.js';
 Doc.UseLibrary( @OnBeforeHtml);

end;

procedure TUIBootstrap.OnBeforeHtml;
begin
  doc.head.SetCssLink('UI-BOOTSTRAP', css);
  doc.head.SetJScriptSrc('ANGULAR', AngularScript);
  doc.head.SetJScriptSrc('UI-BOOTSTRAP', BootStrapScript);
end;




end.

