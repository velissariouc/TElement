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

unit cvElement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, controls;

type

  TCssFont = record
    size, weight, family, style: string;
  end;

  TCssBackgroung = record
    color,
    image,
    repeat_,
    attachment,
    position: string;
  end;
  TElement = class;
  { TCss }

  TCss = class( TObject)
  private
    RawCss: TStringList;
  public
    Selector: string;
    Background: TCssBackgroung;
    font: TCssFont;
    width, height, display, margin, padding: string;
    listStyleType: string;
    //Selector: string;
    function getContent: string;
    function add( aStyle: string): TCss;
    function setBackgroundColor( aValue: string): TCss;
    function setFontFamily( aValue: string): TCss;
    function setFontSize( aValue: string): TCss;
    function setFontWeight( aValue: string): TCss;
    function setMargin( aValue: string): TCss;
    function setPadding( aValue: string): TCss;
    function setListStyleType( aValue: string): TCss;
    constructor create( aSelector: string='');
    constructor create( aSelector: string; owner: TElement);
    destructor Destroy; override;
  end;


  { TElement }

  TElement = class( TObject)
  //private
  private
    fparent: TElement;
    Ftitle: string;
    flevel: integer; //number of parents
    function GetIdAttr: string;
    function getLevel: integer;
    function spacedContent: string;
    function spacedCss: string;
    function spacedCustomContent: string;
    procedure setParent(AValue: TElement);

    //procedure Settitle(AValue: string);
  protected
    // class of the html element
    //FClasse: string;
    // id of the html element
    FId: string;
    // Generated javascript
    FJs: TStringList;
    fJsReady: TStringList; //in DocumentReady


    NoEndingTag: Boolean;
    oneSpace: string;
    space, spacetag: string;
    ClassAtr: TStrings;
    TitleAttr: string;

    function GetId: string; virtual;
    function GetCustomCss: string; virtual;
    function GetClasses: string;
    function GetCustomClasses: string; virtual;
    function GetCustomContent: string; virtual;
    function GetCustomContentAfterChildren: string; virtual;
    function GetExtraAttributes: string; virtual;
    function GetStyle: string;
    function GetTitle: string;
    function getAttributes: string; //without id
  public
    tag: string;
    Css: TCss;
    // HTML content
    Content: TStrings;
    Attributes: TStrings;
    mergeIntoParent, enabled: Boolean;
    children: TList;
    CssList: TList;
    //style
    width,
    height,
    padding, fontSize, textAlign, margin, color
    ,align :string;
    //end of style
    //events
    onClick,
    onKeyPress: string;
    function addContent( aContent: string): TElement;
    function addAttribute( aContent: string): TElement;
    function addJS( aScript: string): TElement;
    function getJs: string; virtual;
    function addJSReady( aScript: string): TElement;
    function getJsReady: string; virtual;
    function setTag( aTag: string): TElement;
    function setTextAlign( aalign: string): TElement;
    function setId( aId: string): TElement;
    function setMargin( aMargin: string): TElement;
    function setPadding( aPadding: string): TElement;
    function setWidth( aWidth: string): TElement;
    function setHeight( aHeight: string): TElement;
    function setColor( aColor: string): TElement;
    function setAlign( aAlign: string): TElement;
    function getCss: string; virtual;
    function response: string;
    function html: string; virtual;
    function AddClass( aClass: string): TElement;
    procedure SaveToFile( aFileName: string);
    function insert( e: Telement): TElement;
    constructor Create( aParent: TElement = nil);
//    constructor Create( aParent: TElement; aTag: string);
    destructor Destroy; override;
    property title: string read TitleAttr write TitleAttr;
    property Id: string read GetId write FId;

    //property Js: TStrings read fJS write fjs;
    property level: integer read flevel;
    property parent: TElement read fparent write setParent;
  end;

  { TDiv }

  TDiv = class( TElement)
  public
    constructor create( aParent: TElement=nil; aClass: string = '');
  end;

  { TButton }

  TButton = class( TElement)
  public
    constructor create( aParent: TElement; aClass: string = '');
  end;

  { TNav }

  TNav = class( TElement)
  public
    constructor create( aParent: TElement; aClass: string = '');
  end;

  { THorizontalNav }

  THorizontalNav = class( TNav)
  protected
    function GetJsReady: string; override;
  public
    constructor create( aParent: TElement);
  end;


  { TParagraph }

  TParagraph = class( TElement)
  public
    constructor create( aParent: TElement; aClass: string = '');
  end;
  { TListItem }

  TListItem = class( TElement)
  public
    constructor create( aParent: TElement; aContent: string = '');
  end;


  { TUnorderedList }

  TUnorderedList = class( TElement)
  public
    constructor create( aParent: TElement; aClass: string = '');
    function AddItem( aContent: string): TUnorderedList;
    procedure AddItem( aContent: string; out AReference);
    function GetItem( cnt: integer): TListItem;
  end;

  { TSlimMenu }

  TSlimMenu = class( TUnorderedList)
  protected
    function GetJsReady: string; override;
  public
    constructor create( aParent: TElement; aClass: string = '');
  end;


  { TSelect }

  TSelect = class( TElement)
  protected
    //options: TList;
  public
    Name: string;
    function setName( aValue: string): TSelect;
    constructor create( aParent: TElement; aClass: string = '');
  end;

  { TOption }

  TOption = class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    Value: string;
    constructor create( aParent: TElement; aValue, aContent: string);
  end;


  { TScript }

  TScript = Class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    src: string; //specifies the URL of an external script file
    constructor create( aParent: TElement);
  end;

  { TLink }
  TLink = class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    href, media, rel, typeAttr: string;
    constructor create( aParent: TElement);
  end;

  { TAnchor }

  TAnchor = class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    href, media, rel, typeAttr: string;
    constructor create( aParent: TElement);
  end;


  { TStyle }

  TStyle = class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    typeAttr: string;
    constructor create( aParent: TElement; aType: string = 'text/css');
  end;

  { THead }

  THead = class( TElement)
  private
    function getTitle: string;
    procedure setTitle(AValue: string);
  protected
    titleElement: TElement;
    metaName, metaContent: TStringList;
    CssLinks, JScriptSrc: TStringList;
    function GetCustomContent: string; override;
  public

    charset: string;
    procedure SetCssLink( aName: string; href: string; media: string ='');
    procedure SetJScriptSrc( aName: string; src: string);
    procedure AddLink( rel, href: string; media: string ='');
    procedure AddCssLink( href: string; media: string ='');
    //function AddCssStyle( href: string; content: string =''): TElement;
    procedure addMeta( aName, aContent: string);
    procedure AddScriptSrc( src: string);
    constructor create( aParent: TElement);
    destructor Destroy; override;
    property Title: string read getTitle write setTitle;
  end;

  { TMeta }

  TMeta = class( TElement)
  protected
    function GetExtraAttributes: string; override;
    function GetExtraMetaContent: string; virtual;
  public
    charset: string;
    name, MetaContent: string;
    constructor create( aParent: TElement);
  end;

  { TFormElement }

  TFormElement = class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    action, method: string;
    constructor create( aParent: TElement);
  end;

  { TInputElement }

  TInputElement = class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    name, value, typeAttr, autocomplete: string;
    function setValue( aValue: string): TInputElement;
    function setName( aValue: string): TInputElement;
    function setAutocomplete( aValue: string): TInputElement;
    constructor create( aParent: TElement; aName: string);
  end;

  { TInputText }

  TInputText = class( TInputElement)
  end;

  { TInputPassword }

  TInputPassword = class( TInputElement)
  public
    constructor create( aParent: TElement; aName: string);
  end;

  { TInputButton }

  TInputButton = class( TInputElement)
  public
    constructor create( aParent: TElement; aName: string);
  end;

  { TInputNumber }

  TInputNumber = class( TInputElement)
  public
    constructor create( aParent: TElement; aName: string);
  end;

  { TLabelElement }

  TLabelElement = class( TElement)
  protected
    function GetExtraAttributes: string; override;
  public
    forElementId: string;
    constructor create( aParent: TElement; aContent: string; forElement: string='');
  end;

  { TFieldSet }

  TFieldSet = class( TElement)
  protected
    function GetExtraAttributes: string; override;
    function GetCustomContent: string; override;
  public
    legend: string;
    constructor create( aParent: TElement; alegend: string='');
  end;



  THtml = class;
  TUseLibrary = procedure of object;

  { THtml }

  THtml = class( TElement)
  private
    Libraries : array of TUseLibrary;
  public
    head: THead;
    body: TElement;
    procedure UseLibrary( aLibraryConfigProcedure: TUseLibrary);
    function html: string; override;
    constructor create( aTitle: string = '');
  end;




  //===================================================================

  TJavaScript = class;

  { TJSVar }

  TJSVar = class
  private
    Fvalue: string;
    procedure Setvalue(AValue: string);
    function getvalue: string;
  public
    //Name: string;
    property value: string read getvalue write Setvalue;
    constructor create( aParent: TJavaScript; aName: string; initValue: variant);

  end;

  { TJavaScript }

  TJavaScript = class
  public
    code: string;
    vars: array of TJSVar;
    VarCounter: integer;
    function NewVar( aName: string; initValue: variant): TJSVar;
    function alert( s: variant):string;
    function alert( s: TJSVar):string;
    constructor create;
  end;


var
  debugStr, debugHtml: TStringList;

implementation
uses
  variants;

{ TButton }

constructor TButton.create(aParent: TElement; aClass: string);
begin
  inherited create( aParent);
  tag := 'button';
  if aClass <> '' then
    AddClass( aClass);

end;



{ TSlimMenu }

function TSlimMenu.GetJsReady: string;
var
  r: string;
begin
  r := '$("ul.slimmenu").slimmenu( '#10+
   '{'#10+
   ' resizeWidth: "800",'#10+
   ' collapserTitle: "Main Menu",'#10+
   ' easingEffect:"easeInOutQuint",'#10+
   ' animSpeed:"medium",'#10+
   ' indentChildren: true,'#10+
   ' childrenIndenter: "&raquo;"'#10+
   '});'#10;
  result:= r +  inherited getJsReady;
end;

constructor TSlimMenu.create(aParent: TElement; aClass: string);
begin
  inherited create( aparent, aClass);
  AddClass('slimmenu');
end;

{ THorizontalNav }

function THorizontalNav.GetJsReady: string;
var
  r: string;
begin
  r := '$(".ful-width").horizontalNav();'#10;
  result:= r +  inherited getJsReady;
end;

constructor THorizontalNav.create(aParent: TElement);
begin
  inherited create( aparent, 'horizontal-nav ful-width');
end;

{ TAnchor }

function TAnchor.GetExtraAttributes: string;
var
  s: string;
begin
  s := '';
  if rel <> '' then
    s := ' rel="' + rel + '"';
  if typeAttr <> '' then
    s := s + ' type="' + typeAttr + '"';
  if media <> '' then
    s := s + ' media="' + media + '"';
  if href <> '' then
    s := s + ' href="' + href + '"';
  result := s;
end;

constructor TAnchor.create(aParent: TElement);
begin
  inherited create( aparent);
  tag := 'a';
end;

{ TNav }

constructor TNav.create(aParent: TElement; aClass: string);
begin
  inherited create( aParent);
  tag := 'nav';
  if aClass <> '' then
    AddClass( aClass);
end;

{ TJavaScript }

function TJavaScript.NewVar(aName: string; initValue: variant): TJSVar;
begin
  inc( VarCounter);
  SetLength( Vars, VarCounter);
  Vars[ VarCounter - 1] := TJSVar.create( self, aName, initValue);
  result := Vars[ VarCounter - 1] ;
end;

constructor TJavaScript.create;
begin
  VarCounter:=0;
end;

{ TJSVar }

procedure ShowBasicVariantType(varVar: Variant);
var
  typeString : string;
  basicType  : Integer;

begin
  // Get the Variant basic type :
  // this means excluding array or indirection modifiers
  basicType := VarType(varVar) and VarTypeMask;

  // Set a string to match the type
  case basicType of
    varEmpty     : typeString := 'varEmpty';
    varNull      : typeString := 'varNull';
    varSmallInt  : typeString := 'varSmallInt';
    varInteger   : typeString := 'varInteger';
    varSingle    : typeString := 'varSingle';
    varDouble    : typeString := 'varDouble';
    varCurrency  : typeString := 'varCurrency';
    varDate      : typeString := 'varDate';
    varOleStr    : typeString := 'varOleStr';
    varDispatch  : typeString := 'varDispatch';
    varError     : typeString := 'varError';
    varBoolean   : typeString := 'varBoolean';
    varVariant   : typeString := 'varVariant';
    varUnknown   : typeString := 'varUnknown';
    varByte      : typeString := 'varByte';
    varWord      : typeString := 'varWord';
    varLongWord  : typeString := 'varLongWord';
    varInt64     : typeString := 'varInt64';
    varStrArg    : typeString := 'varStrArg';
    varString    : typeString := 'varString';
    varAny       : typeString := 'varAny';
    varTypeMask  : typeString := 'varTypeMask';
  end;

  // Show the Variant type
  //ShowMessage('Variant type  = '+typeString);
end;

procedure TJSVar.Setvalue(AValue: string);
begin
  if Fvalue=AValue then Exit;
  Fvalue:=AValue;
end;

function TJSVar.getvalue: string;
begin
  result := fvalue;
end;

constructor TJSVar.create(aParent: TJavaScript; aName: string; initValue: variant);
var
  basicType  : Integer;
begin
   //ShowBasicVariantType(initValue);
  aParent.Code := aParent.Code + 'var ' + aName;
  fValue := aName;
  basicType := VarType(initValue) and VarTypeMask;
   case basicType of
    //varEmpty     : typeString := 'varEmpty';
    //varNull      : typeString := 'varNull';
    varSmallInt,varshortint,
    varInteger,
    varByte,
    varWord,
    varLongWord,
    varInt64     :
    aParent.Code :=  aParent.Code + ' = ' + IntToStr( initValue);

    //varSingle    : typeString := 'varSingle';
    //varDouble    : typeString := 'varDouble';
    //varCurrency  : typeString := 'varCurrency';
    //varDate      : typeString := 'varDate';
    //varOleStr    : typeString := 'varOleStr';
    //varDispatch  : typeString := 'varDispatch';
    //varError     : typeString := 'varError';
    //varBoolean   : typeString := 'varBoolean';
    //varVariant   : typeString := 'varVariant';
    //varUnknown   : typeString := 'varUnknown';
    varStrArg,
    varString    :
       if initvalue <> '' then  aParent.Code :=  aParent.Code + ' = "' + initValue + '"';

    //varAny       : typeString := 'varAny';
    //varTypeMask  : typeString := 'varTypeMask';
  end;


  //if initValue <> '' then
  //  aParent.Code :=  aParent.Code + ' = ' + initValue;
  aParent.Code :=  aParent.Code + ';'#10#13;
end;

function  TJavaScript.alert(s: variant): string;
var
  basicType  : Integer;
  //ccode: string;
begin
    result :=  'alert (';
 // if s is TJSVar then
   // code := code +TJSVar(s).Name;

  //ShowBasicVariantType(s);
  //Code := Code + 'var ' + aName;

  basicType := VarType(s) and VarTypeMask;
   case basicType of
    //varEmpty     : typeString := 'varEmpty';
    //varNull      : typeString := 'varNull';
    varSmallInt,varshortint,
    varInteger,
    varByte,
    varWord,
    varLongWord,
    varInt64     :
    result :=  result + IntToStr( s);

    //varSingle    : typeString := 'varSingle';
    //varDouble    : typeString := 'varDouble';
    //varCurrency  : typeString := 'varCurrency';
    //varDate      : typeString := 'varDate';
    //varOleStr    : typeString := 'varOleStr';
    //varDispatch  : typeString := 'varDispatch';
    //varError     : typeString := 'varError';
    //varBoolean   : typeString := 'varBoolean';
    //varVariant   : typeString := 'varVariant';
    //varUnknown   : typeString := 'varUnknown';
    varStrArg,
    varString    :
       if s <> '' then  result :=  result + ' "' + s + '"';
   end;


  result := result + '");'+#10#13;
  code := code + ' ' + result;
end;

function TJavaScript.alert(s: TJSVar): string;
begin
  result := 'alert('+ s.value+');';
  code := code + result;
end;

{ TListItem }

constructor TListItem.create(aParent: TElement; aContent: string);
begin
  inherited create( aParent);
  tag := 'li';
  if aContent <> '' then
    addContent( aContent);
end;

{ TUnorderedList }

constructor TUnorderedList.create(aParent: TElement; aClass: string);
begin
  inherited create( aParent);
  tag := 'ul';
  if aClass <> '' then
    AddClass( aClass);
end;

function TUnorderedList.AddItem(aContent: string): TUnorderedList;
begin
  TListItem.create( self, aContent);
  result := self;
end;

procedure TUnorderedList.AddItem(aContent: string; out AReference);
var
  VReference: TListItem absolute AReference;
begin
  VReference := TListItem.create( self, aContent);
  //result := self;
end;

function TUnorderedList.GetItem(cnt: integer): TListItem;
begin
  if (cnt < 0) or ( cnt >= children.Count) then
    result := nil
  else
    result := TListItem( children.Items[cnt]);
end;

{ TOption }

function TOption.GetExtraAttributes: string;
var
  s: string;
begin
  inherited GetExtraAttributes;
  if value <> '' then
    s := s + ' value="'+ value + '"';
  result := s;
end;


constructor TOption.create(aParent: TElement; aValue, aContent: string);
begin
  inherited create( aParent);
  //options := TList.Create;
  tag := 'option';
  addContent( aContent);
  Value := aValue;
end;

{ TSelect }

function TSelect.setName(aValue: string): TSelect;
begin
  Name := aValue;
  result := self;
end;

constructor TSelect.create(aParent: TElement; aClass: string);
begin
  inherited create( aParent);
  //options := TList.Create;
  tag := 'select';
  if aClass <> '' then
    AddClass( aClass);
end;

{ TParagraph }

constructor TParagraph.create(aParent: TElement; aClass: string);
begin
  inherited create( aParent);
  tag := 'p';
  if aClass <> '' then
    AddClass( aClass);
end;


{ TInputNumber }

constructor TInputNumber.create(aParent: TElement; aName: string);
begin
  inherited create(aParent, aName);
  typeAttr := 'number';
end;


{ TCss }

function TCss.getContent: string;
var
  s: string;
begin
  if Background.attachment <> '' then
    s := s + 'background-attachment:' + Background.attachment + ';'#10;
  if Background.color <> '' then
    s := s + 'background-color:' + Background.color + ';'#10;
  if Background.image <> '' then
    s := s + 'background-image:' + Background.image + ';'#10;
  if Background.position <> '' then
    s := s + 'background-position:' + Background.position + ';'#10;
  if Background.repeat_ <> '' then
    s := s + 'background-repeat:' + Background.repeat_ + ';'#10;

  if font.family <> '' then
    s := s + 'font-family:' + font.family + ';'#10;
  if font.style <> '' then
    s := s + 'font-style:' + font.style + ';'#10;
  if font.size <> '' then
    s := s + 'font-size:' + font.size + ';'#10;
  if font.weight <> '' then
    s := s + 'font-weight:' + font.weight + ';'#10;


  if width <> '' then
    s := s + 'width:' + width + ';'#10;
  if height <> '' then
    s := s + 'height:' + height + ';'#10;
  if display <> '' then
    s := s + 'display:' + display + ';'#10;
  if margin <> '' then
    s := s + 'margin:' + margin + ';'#10;
  if padding <> '' then
    s := s + 'padding:' + padding + ';'#10;
  if listStyleType <> '' then
    s := s + 'list-style-type:' + listStyleType + ';'#10;

  //s := s + RawCss.Text;

  if s <> '' then
    if (selector <> '') then
      s := Selector +' {'#10 + s + '}'#10
    else
      s := '{'#10 + s + '}'#10;

  result := s;
end;

function TCss.add(aStyle: string): TCss;
begin
  RawCss.Add( aStyle);
  result := self;
end;

function TCss.setBackgroundColor(aValue: string): TCss;
begin
  Background.color := aValue;
  result := self;
end;

function TCss.setFontFamily(aValue: string): TCss;
begin
  font.family:=aValue;
  result := self;
end;

function TCss.setFontSize(aValue: string): TCss;
begin
  font.size:=aValue;
  result := self;
end;

function TCss.setFontWeight(aValue: string): TCss;
begin
  font.weight:=aValue;
  result := self;
end;

function TCss.setMargin(aValue: string): TCss;
begin
  margin:=aValue;
  result := self;
end;

function TCss.setPadding(aValue: string): TCss;
begin
  padding:=aValue;
  result := self;
end;

function TCss.setListStyleType(aValue: string): TCss;
begin
  listStyleType:=aValue;
  result := self;
end;

constructor TCss.create(aSelector: string='');
begin
  inherited create;
  RawCss := TStringList.Create;
  Selector := aSelector;
end;

constructor TCss.create(aSelector: string; owner: TElement);
begin
  create( aSelector);
  if owner <> nil then
  begin
    if owner.tag = 'html' then
      owner := THtml(owner).body;
    owner.CssList.Add( self);
  end;
end;

destructor TCss.Destroy;
begin
  RawCss.Free;
  inherited Destroy;
end;

{ TInputElement }

function TInputElement.GetExtraAttributes: string;
var
  s: string;
begin
  inherited GetExtraAttributes;
  s := ' type="'+typeAttr+'"';
  if name <> '' then
    s := s + ' name="'+ name + '"';
  if value <> '' then
    s := s + ' value="'+ value + '"';
  if autocomplete <> '' then
    s := s + ' autocomplete="'+ autocomplete + '"';

  result := s;
end;

function TInputElement.setValue(aValue: string): TInputElement;
begin
  value := aValue;
  result := self;
end;

function TInputElement.setName(aValue: string): TInputElement;
begin
  name := aValue;
  result := self;
end;

function TInputElement.setAutocomplete(aValue: string): TInputElement;
begin
  autocomplete := aValue;
  result := self;
end;

constructor TInputElement.create(aParent: TElement; aName: string);
begin
  inherited create( aParent);
  tag := 'input';
  typeAttr:='text';
  name := aName;
  id := aName;
  NoEndingTag:=true;
end;

{ TInputButton }




constructor TInputButton.create(aParent: TElement; aName: string);
begin
  inherited create(aParent, aName);
  typeAttr := 'button';
end;

{ TFieldSet }

function TFieldSet.GetExtraAttributes: string;
var
  s: string;
begin
  s := '';
  //if legend <> '' then
  //  s := s + ' for="'+ forElementId + '"';
  result := s;
end;

function TFieldSet.GetCustomContent: string;
begin
  Result:=inherited GetCustomContent;
  if legend <> '' then
    addContent('<legend>'+legend+'</legend>');
end;

constructor TFieldSet.create(aParent: TElement; aLegend: string ='');
begin
  inherited create(aParent);
  tag := 'fieldset';
  legend := aLegend;
end;

{ TLabelElement }

function TLabelElement.GetExtraAttributes: string;
var
  s: string;
begin
  if forElementId <> '' then
    s := s + ' for="'+ forElementId + '"';
  result := s;
end;

constructor TLabelElement.create(aParent: TElement; aContent: string; forElement: string='');
begin
  inherited create(aParent);
  tag := 'label';
  forElementId:= forElement;
  if aContent <> '' then
    addContent( aContent);
end;

{ TInputPassword }


constructor TInputPassword.create(aParent: TElement; aName: string);
begin
  inherited create(aParent, aName);
  typeAttr := 'password';
end;

{ TFormElement }

function TFormElement.GetExtraAttributes: string;
var
  s: string;
begin
  s := ' method="' + method + '"';
  if action <> '' then
      s := s + ' action="' + action + '"';
  result := s;
end;

constructor TFormElement.create(aParent: TElement);
begin
  inherited create(aParent);
  tag := 'form';
  method:='post';

end;

{ TMeta }

function TMeta.GetExtraAttributes: string;
var
  s, ec, coma: string;
begin
  s := '';
  if name <> '' then
      s := s + ' name="' + name + '"';
  ec := GetExtraMetaContent;

  if (MetaContent <> '') and (ec <> '') then
    coma := ','
  else
    coma := '';
  if (MetaContent <> '') or (ec <> '') then
      s := s + ' content="' + MetaContent + coma + ec +'"';
  result := s;
end;

function TMeta.GetExtraMetaContent: string;
begin
  result := '';
end;

constructor TMeta.create(aParent: TElement);
begin
  if (aParent = nil) or not (aParent  is THead) then
    raise Exception.Create('<meta> must be defined inside a <head>');
  inherited create(aParent);
  tag := 'meta';
  NoEndingTag:= true;
  //charset:='UTF-8';
end;

{ TStyle }

function TStyle.GetExtraAttributes: string;
var
  s: string;
begin
  if typeAttr <> '' then
      s := s + ' type="' + typeAttr + '"';
  result := s;
end;

constructor TStyle.create(aParent: TElement; aType:string);
begin
  inherited create( aparent);
  tag := 'style';
  typeAttr := aType;

end;

{ TScript }

function TScript.GetExtraAttributes: string;
var
  s: string;
begin
  s := '';
  if src <> '' then
      s := s + ' src="' + src + '"';

  result := s;
end;

constructor TScript.create(aParent: TElement);
begin
  inherited create( aparent);
  tag := 'script';
end;

{ THead }

function THead.getTitle: string;
begin
  result := titleElement.Content.Text;
end;

procedure THead.setTitle(AValue: string);
begin
  titleElement.Content.Text:=avalue;
end;

function THead.GetCustomContent: string;
var
  s: string;
  i: integer;
begin
  if charset <> '' then
    s := '<meta charset="' + charset + '">'#10;

  for i :=0 to metaName.Count - 1 do
    s:= s + '<meta name="' + metaName[i] + '" content="' + metaContent[i] + '">'#10;
  result := s;
end;

procedure THead.SetCssLink(aName: string; href: string; media: string);
var
  index: integer;
  l: TLink;
begin
  index := CssLinks.IndexOf( aName);
  if index = -1 then
  begin
    l := TLink.Create( self);
    l.href:= href;
    l.media:=media;
    l.typeAttr:='text/css';
    l.rel:='stylesheet';
    Csslinks.AddObject( aName, l);
  end
  else
  begin
    TLink( CssLinks.Objects[index]).href:= href;
    TLink( CssLinks.Objects[index]).media:= media;
  end;
end;

procedure THead.SetJScriptSrc(aName: string; src: string);
var
  index: integer;
  e: TScript;
begin
  index := JScriptSrc.IndexOf( aName);
  if index = -1 then
  begin
    e := TScript.Create( self);
    e.src:= src;
    JScriptSrc.AddObject( aName, e);
  end
  else
  begin
    TScript( JScriptSrc.Objects[index]).src := src;
  end;
end;

procedure THead.AddLink(rel, href: string; media: string);
var
  l: TLink;
begin
  l := TLink.Create( self);
  l.href:= href;
  l.media:=media;
end;

procedure THead.AddCssLink(href: string; media: string);
var
  l: TLink;
begin
  l := TLink.Create( self);
  l.href:= href;
  l.media:=media;
  l.typeAttr:='text/css';
  l.rel:='stylesheet';
end;

procedure THead.addMeta( aName, aContent: string);
begin
  metaName.Add( aName);
  metaContent.add( aContent);
end;

{function THead.AddCssStyle(href: string; content: string): TElement;
begin
  inherited create( aparent);
  tag := 'style';

end;
 }
procedure THead.AddScriptSrc(src: string);
var
  e: TScript;
begin
  e := TScript.Create( self);
  e.src:= src;
end;

constructor THead.create(aParent: TElement);
begin
  inherited create(aParent);
  tag := 'head';
  titleElement := TElement.Create( self);
  titleElement.tag := 'title';
  charset := 'UTF-8';
  metaName := TStringList.Create;
  metaContent := TStringList.Create;
  CssLinks := TStringList.Create;
  JScriptSrc := TStringList.Create;
end;

destructor THead.Destroy;
begin
  metaContent.Free;
  metaName.Free;
  CssLinks.Free;
  JScriptSrc.Free;
  inherited Destroy;
end;

{ TLink }

function TLink.GetExtraAttributes: string;
var
  s: string;
begin
  s := ' rel="' + rel + '"';
  if typeAttr <> '' then
      s := s + ' type="' + typeAttr + '"';
  if media <> '' then
      s := s + ' media="' + media + '"';
  if href <> '' then
      s := s + ' href="' + href + '"';
  result := s;
end;

constructor TLink.create(aParent: TElement);
begin
  if (aParent = nil) or not(aParent is THead) then
    raise exception.create('a <link> must be inside a <head>');
  inherited create( aparent);
  tag := 'link';
  NoEndingTag:= true;
end;

{ THtml }


procedure THtml.UseLibrary( aLibraryConfigProcedure: TUseLibrary);
var
  i: integer;
begin
  i := Length( Libraries);
  SetLength( Libraries, i + 1);
  Libraries[i] := aLibraryConfigProcedure;
end;

function THtml.html: string;
var
  s, strCss, strJS, a, b: string;
  Style: TStyle;
  Script: TScript;
  index: integer;
begin
  for index := 0 to Length( Libraries) - 1 do
    Libraries[index]();
    //Libraries[index]( self);
  Style := nil;
  Script := nil;
  strCss := body.spacedCss;
  //strCss := body.getcss;
  //showmessage( strCss);
  a := body.getJs;
  b := body.getjsReady;
  strJS := a + b;
  if strCss <> '' then
  begin
    style := TStyle.create( head);
    style.Content.Add( strCss);
  end;
  if strJS <> '' then
  begin
    Script := TScript.create( head);
    Script.Content.Add( strJS);
  end;
  {s := '<!DOCTYPE html>'#10 +
  '<HTML>'#10+
  head.html +
  body.html +
  '</HTML>'#10;  }

    s := '<!DOCTYPE html>'#10 +  inherited;

  result := s;
  if style <> nil then
  begin
    index := head.children.IndexOf( style);
    head.children.Delete( index);
    style.free;
  end;
  if script <> nil then
  begin
    index := head.children.IndexOf( script);
    head.children.Delete( index);
    script.free;
  end;
end;

constructor THtml.create(aTitle: string);
begin
  inherited create( nil);
  tag := 'html';
  head := THead.Create( self);
  head.title := aTitle;

  body := TElement.Create( self);
  body.tag := 'body';
end;



{ TDiv }

constructor TDiv.create(aParent: TElement; aClass: string = '');
begin
  inherited create( aParent);
  tag := 'DIV';
  if aClass <> '' then
    AddClass( aClass);
end;


{ TElement }

function TElement.html: string;
var
  s, ChildrenContent, MergedTag: string;
  i: integer;
  el: TElement;

begin
  debughtml.add('======================');
  debughtml.add('enter ' + tag);
  if not enabled then
  begin
    result := '';
    exit;
  end;

  if NoEndingTag then
  begin
    s := spacetag + '<'+tag + GetIdAttr + getAttributes + GetClasses + GetExtraAttributes
    +GetStyle + '/>'#10;
    result := s;
    exit;
  end;


  s := '';
  ChildrenContent := '';
  MergedTag := '';
  for i:=0 to children.count -1 do
  begin
    el := TElement( children[i]);
    ChildrenContent := ChildrenContent + el.html;
    if el.mergeIntoParent then
      MergedTag := MergedTag + el.getAttributes + el.GetClasses + el.GetStyle + el.GetExtraAttributes;
  end;

  if not mergeIntoParent then
    s := spacetag + '<'+tag + GetIdAttr + getAttributes + GetClasses + GetExtraAttributes
    +GetStyle + MergedTag + '>'#10;

  s := s + spacedCustomContent +
       spacedContent +
       ChildrenContent +
       GetCustomContentAfterChildren;

  if not mergeIntoParent then
    s := s + spacetag + '</'+tag+'>'#10;

  result := s;
  debughtml.add(s);
//  debughtml.add('======================');
  debughtml.add('EXIT ' + tag + #10);

end;


function TElement.GetId: string;
begin
  result := fid;
end;

function TElement.GetIdAttr: string;
begin
    if fid <> '' then
    result := ' id="' + fid + '" ';

end;

function TElement.getJsReady: string;
var
  i: integer;
  r, beginstr, endstr: string;
begin
  r := '';
  beginstr := '';
  endstr := '';
  if not enabled then
  begin
    result := '';
    exit;
  end;

  if (parent = nil) or (parent.tag='html') then
    beginstr := '$(function() {'#10;

  r := r + fJSReady.Text;

  for i:=0 to children.count - 1 do
  begin
     r := r + TElement( children[i]).getjsReady;
  end;

  if (parent = nil) or (parent.tag='html') then
    endstr := '});'#10;
  if r <> '' then
    r := beginstr + r + endstr;
  result := r;
end;

function TElement.setTag(aTag: string): TElement;
begin
  Tag := aTag;
  result := self;
end;

function TElement.getCss: string;
var
  i: integer;
  r: string;
  cssContent: string;
begin
  debugstr.Add( tag + ' enter getCss');
  if not enabled then
  begin
    result := '';
    exit;
  end;

  if parent = nil then
    r := '<STYLE type="text/css"> '#10;

  cssContent := css.getContent;
  if cssContent <> '' then
  begin
    if fid <> '' then
      cssContent := '#' + fid + ' ' + cssContent
    else
      cssContent := tag + ' ' + cssContent
  end;

  r := r + GetCustomCss + cssContent + css.RawCss.Text;

  for i:=0 to children.count - 1 do
  begin
     r := r + TElement( children[i]).getcss;
  end;

  for i := 0 to CssList.Count - 1 do
  begin
    r := r + TCss( CssList[i]).getContent;
  end;

  if parent = nil then
    r := r + '</STYLE>'#10;

  result := r;
    debugstr.Add( r );
  debugstr.Add( tag + ' exit getCss'#10#10);
end;



function TElement.GetCustomCss: string;
begin

end;

function TElement.GetClasses: string;
var
  s: string;
  i: integer;
begin
  s:='';
  for i:=0 to ClassAtr.Count - 1 do
    s:= s + ClassAtr[i] + ' ';
  s := s + GetCustomClasses;
  if s <> '' then
    s := ' class="'+s+'" ';
  result := s;
end;

function TElement.GetCustomClasses: string;
begin
  result := '';
end;

function TElement.GetCustomContent: string;
begin
  result := '';
end;

function TElement.GetCustomContentAfterChildren: string;
begin
  result := '';
end;

function TElement.GetExtraAttributes: string;
begin
  result := '';
end;

function TElement.GetStyle: string;
var
  style: string;
begin
  style := '';
  if width <> '' then
    style := style + 'width:' + width + ';';
  if height <> '' then
    style := style + 'height:' + height +';';
  if padding <> '' then
    style := style + 'padding:' + padding +';';
  if fontSize <> '' then
    style := style + 'font-size:' + fontSize +';';
  if textAlign <> '' then
    style := style + 'text-align:' + textAlign +';';
  if margin <> '' then
    style := style + 'margin:' + margin +';';
  if color <> '' then
    style := style + 'color:' + color +';';
  if align <> '' then
    style := style + 'align:' + align +';';

  if style <> '' then
    style := ' style="'+style+'"';
  result := style;
end;

function TElement.GetTitle: string;
begin
  result := '';
  if titleattr <> '' then
    result := ' title="'+titleattr+'" ';
end;

function TElement.getAttributes: string;
var
  s: string;
begin

  s := ' ' + Attributes.Text + GetTitle;
  if onClick <> '' then
    s := s + ' onclick="'+onClick+'" ';
  if onKeyPress <> '' then
    s := s + ' onkeypress="'+onKeyPress+'" ';
  result := s;
end;

function TElement.addContent(aContent: string): TElement;
begin
  content.Add( aContent);
  result := self;
end;

function TElement.addAttribute(aContent: string): TElement;
begin
  Attributes.Text := Attributes.Text +' ' + aContent;
  result := self;
end;

function TElement.addJS(aScript: string): TElement;
begin
  fjs.Add( aScript);
  result := self;
end;

function TElement.getJs: string;
var
  s: string;
  i: integer;
begin
  s := fjs.text;
  for i:=0 to children.count - 1 do
  begin
    s := s + TElement( children[i]).getjs;
  end;
  result := s;
end;

function TElement.addJSReady(aScript: string): TElement;
begin
  fjsReady.Add( aScript);
  result := self;
end;

function TElement.setTextAlign(aalign: string): TElement;
begin
  textAlign:=aalign;
  result := self;
end;

function TElement.setId(aId: string): TElement;
begin
  fid := aid;
  result := self;
end;

function TElement.setMargin(aMargin: string): TElement;
begin
  margin := aMargin;
  result := self;
end;

function TElement.setPadding(aPadding: string): TElement;
begin
  padding := aPadding;
  result := self;
end;

function TElement.setWidth(aWidth: string): TElement;
begin
  css.width := awidth;
  result := self;
end;

function TElement.setHeight(aHeight: string): TElement;
begin
  css.height := aHeight;
  result := self;
end;

function TElement.setColor(aColor: string): TElement;
begin
  color := aColor;
  result := self;
end;

function TElement.setAlign(aAlign: string): TElement;
begin
  align := aAlign;
  result := self;
end;

function TElement.getLevel: integer;
var
  l: integer;
  el: TElement;
begin
  l := 0;
  el := self;
  while el.parent <> nil do
  begin
    inc( l);
    el := el.parent;
  end;
  result := l;
end;

function TElement.spacedContent: string;
var
  i: integer;
  s: string;
begin
  s := '';
  for i := 0 to Content.Count - 1 do
    s := s + space + Content[i]+#10;
  result := s;
end;

function TElement.spacedCss: string;
var
  i: integer;
  s: string;
  sl:TStringList;
begin
  sl:=TStringList.create;
  sl.text := getCss;
  for i := 0 to sl.Count - 1 do
    s := s + space + sl[i]+#10;
  result := s;
  sl.free;
end;

function TElement.spacedCustomContent: string;
var
  sl: TStringList;
  i: integer;
  s: string;
begin
  sl := TStringList.Create;
  sl.Text:= GetCustomContent;
  s:='';
  for i := 0  to sl.Count - 1 do
    s := s + space + sl[i] + #10;
  sl.free;
  result := s;
end;

procedure TElement.setParent(AValue: TElement);
var
  i: integer;
begin
  //if fparent=AValue then Exit;
  fparent:=AValue;
  fLevel := getLevel;
  spacetag := '';
  for i:= 1 to flevel do
    spacetag := spacetag + oneSpace;
  space := spacetag + oneSpace;
end;

function TElement.response: string;
begin
  result :=
    '<!DOCTYPE html>'#10 +
    '<html>'#10 +
    '<head>'#10 +
    getCss +
    getJs +
    getJsReady +
    '</head>'#10 +
    html +
    '</html>'#10;;
end;

function TElement.AddClass(aClass: string):TElement;
begin
  ClassAtr.Add( aClass);
  result := self;
end;

procedure TElement.SaveToFile(aFileName: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.Text:= html;
  s.SaveToFile( aFileName);
  s.free;
end;

function TElement.insert(e: Telement): TElement;
begin
  e.parent := self;
  children.Add( e);
  result := self;
end;

constructor TElement.Create( aParent: TElement = nil);
begin
  inherited create;
  NoEndingTag := False;
  oneSpace:='  ';
  enabled := true;
  Content:= TStringList.Create;
  Attributes := TStringList.Create;
  FJs:= TStringList.Create;
  FJsReady:= TStringList.Create;
  Css:= TCss.Create;
  ClassAtr:= TStringList.Create;
  parent := aParent;
  if parent <> nil then
    parent.children.Add( self);
  children := TList.Create;
  CssList := TList.Create;
  mergeIntoParent:=false;
end;

//constructor TElement.Create(aParent: TElement; aTag: string);
//begin
//  create( aParent);
//  Tag := aTag;
//end;

destructor TElement.Destroy;
var
  i: integer;
begin
  ClassAtr.Free;
  Content.Free;
  Attributes.Free;
  FJs.Free;
  fjsready.free;
  Css.Free;
  if children <> nil then
  begin
    for i:= children.Count - 1 downto 0 do
      TElement( children[i]).Free;
    children.free;
  end;
  if CssList <> nil then
  begin
    for i:= CssList.Count - 1 downto 0 do
      TCss( CssList[i]).Free;
    CssList.free;
  end;
  inherited Destroy;
end;

initialization
  debugStr :=  TStringList.Create;
  debugHtml :=  TStringList.Create;

finalization
  debugStr.Free;
  debugHtml.free;
end.

