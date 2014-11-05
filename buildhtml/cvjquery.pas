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
unit cvJQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, cvElement;

type
{ TjqPane }

TjqPane = class( TDiv)
protected
  function GetId: string; override;
  function GetCustomClasses: string; override;
  function GetOptionsJs: string;
public
  kind:string;
  size: string;
  resizable,
  closable,
  initClosed: boolean;
  spacingOpen, //Spacing between pane and adjacent pane - when pane is 'open' or 'closed'
  spacingClosed: integer;
  constructor create( aParent: TElement; akind: string);
end;

{ TJqLayout }

TJqLayout = class(TDiv)
protected
  function getJsReady: string; override;
public
  north, south, center, east, west: TJqPane;
  constructor create( aParent: TElement; aId: string);
end;

THeightStyle = (hsAuto, hsFill, hsContent);
{
heightStyleType: String
Default: "content"
Controls the height of the tabs widget and each panel. Possible values:
    "auto": All panels will be set to the height of the tallest panel.
    "fill": Expand to the available height based on the tabs' parent height.
    "content": Each panel will be only as tall as its content.
}

{ TjqTabPanel }

TjqTabPanel = class( TDiv)
protected
  function GetLink: string;

public
  TabIndex: integer;
  Caption: string;
  AjaxURL: string;
  function html: string; override;

  constructor create( aParent: TElement; aId: string);
end;

{ TjqTabs }

TjqTabs = class( TDiv)
protected
  FIsCollapsible: boolean;
  FOpenOnMouseOver: boolean;
  TabList: TList;
  function GetCustomContent: string; override;
  function GetCustomContentAfterChildren: string; override;
  function GetJsReady: string; override;
  function GetHeightStyleJS: string;
  function GetCustomCss: string; override;
public
  HeightStyle: THeightStyle;
  (*li.ui-state-default {
font-size : 10px;
}
div.ui-tabs-panel {
Formatting Content | 13
font-size : 15px;
font-family : georgia;
font-style : italic;
}
*)
  LinkFontSize: string;
  onBeforeLoad: string;
  function AddTab( aCaption: string; aId: string = ''): TjqTabPanel;
  constructor create( aParent: TElement; aId: string);
  destructor destroy; override;
  property Collapsible: boolean read FIsCollapsible write FIsCollapsible;
  property OpenOnMouseOver: boolean read FOpenOnMouseOver write FOpenOnMouseOver;
end;

{ TjqDialog }

TjqDialog = class( TDiv)
  protected
    function GetJsReady: string; override;
public
  //Title: string;
  Modal: Boolean;
  //Height, Width: integer;
  function html: string; override;
  function setTitle( aTitle: string): TjqDialog;
  function setModal( aModal: Boolean): TjqDialog;
  function setHeight( aHeight: integer): TjqDialog;
  function setWidth( aWidth: integer): TjqDialog;
  constructor create( aParent: TElement; aId: string);
end;

{ TjqDatePicker }

TjqDatePicker = class( TInputElement)
protected
  function GetJsReady: string; override;
public
  DateFormat: string;
  function setDateFormat( aDateFormat: string): TjqDatePicker;
end;

TDataType = (
          { XML format }
          dtXml,
          { Json format }
          dtJson);

{ Type of Ajax call when asking data from server}
TAjaxCall = (
          { Post method }
          acPost,
          { Get method }
          acGet);

{ Type of alignement }
TColAlign = (
          { Align left }
          caLeft,
          { Align right }
          caRight,
          { Align center }
          caCenter);


TjsonReader = record
  total: string;   //total pages for the query
  page: string;    //current page of the query
  records: string; //total number of records for the query
  rows: string;    //an array that contains the actual data
  id: string;      //the unique id of the row
  root: string;    //This element describes where our data begins.
                   //In other words, this points to the array that contains
                   //the data. If we set
  cell: string;    //an array that contains the data for a row
  repeatitems: boolean; //Setting this option to false instructs jqGrid to
                        //search elements in the json data by name
end;


{ TColumn }

TColumn = class(TObject)
private
  FTitle: string;
  FName: string;
  FWidth: integer;
  FAlign: TColAlign;
  FSortable: boolean;
public
  sorttype: string;
  hidden: boolean;
  editable: boolean;
  key: boolean; //if there is no id from server, this can be set as as id for
  //the unique row id. Only one column can have this property. If there are more than one key the grid finds the first one and the second is ignored.
  { Title of the column }
  property Title: string read FTitle write FTitle;
  { Name of the data column }
  property Name: string read FName write FName;
  { Width column }
  property Width: integer read FWidth write FWidth;
  { Alignement Defines the alignment of the cell in the Body layer,
    not in header cell. Possible values: left, center, right.}
  property Align: TColAlign read FAlign write FAlign;
  { Define if the column is sortable }
  property Sortable: boolean read FSortable write FSortable;
  constructor create;
end;

TTreeGridModel = (tgAdjacency, tgNested);
{ TjqGrid }

TjqGrid = class( TElement)
private
  FColumns: TObjectList;
  fHeightJs: string;
protected
  function GetJsReady: string; override;
  function GetDatatype: string;
  function GetUrl: string;
  function getEditUrl: string;
  function GetHeightJs: string;
  function GetGridView: string;
  function getShrinkToFit: string;
  function GettreeGrid: string;
  function GetJasonReader: string;
  function GetAutoWidth: string;
  function getExpandColumn: string;
  function getEvents: string;
  function getPager: string;
  function GetCustomContent: string; override;
public
  jsonReader: TjsonReader;
  autowidth: boolean; {When set to true, the grid width is recalculated
  automatically to the width of the parent element. This is done only initially
  when the grid is created. In order to resize the grid when the parent element
  changes width you should apply custom code and use the setGridWidth method for
  this purpose}
  treeGridModel: TTreeGridModel;
  ExpandColumn: string;//ndicates which column (name from colModel) should be used to expand the tree grid. If not set the first one is used. Valid only when treeGrid option is set to true.
  DataType: TDataType;
  url, editurl: string;
  HasPager: Boolean;
  gridView: boolean;
  {In the previous versions of jqGrid including 3.4.X, reading a relatively large
  data set (number of rows >= 100 ) caused speed problems. The reason for this
  was that as every cell was inserted into the grid we applied about 5 to 6
  jQuery calls to it. Now this problem is resolved; we now insert the entry row
  at once with a jQuery append. The result is impressive - about 3 to 5 times
  faster. What will be the result if we insert all the data at once?
  Yes, this can be done with a help of gridview option (set it to true).
  The result is a grid that is 5 to 10 times faster. Of course, when this option
  is set to true we have some limitations. If set to true we can not use treeGrid,
  subGrid, or the afterInsertRow event. If you do not use these three options
  in the grid you can set this option to true and enjoy the speed.
  }
  treeGrid: boolean;
  shrinkToFit: boolean;
  onSelectRow, onLoadComplete : string;
  property height: string read  fHeightJs write fHeightJs;
  function AddColumn: TColumn;
  procedure AddColumn(ATitle: string; AName: string; AWidth: integer; AALign: TColAlign; AIsSortable: boolean);
  constructor create( aParent: TElement; aId: string);
  destructor Destroy; override;
end;


implementation

{ TjqDatePicker }

function TjqDatePicker.GetJsReady: string;
var
  r: string;
begin
  r := '$("#' + id + '").datepicker({'#10;
  if DateFormat<>'' then
    r := r + 'dateFormat: "' + DateFormat + '"'#10;
  r := r +  '});'#10 +
         #10;

   result:= r +  inherited getJsReady;
end;

function TjqDatePicker.setDateFormat(aDateFormat: string): TjqDatePicker;
begin
  DateFormat := aDateFormat;
  result := self;
end;


{ TjqDialog }

function TjqDialog.GetJsReady: string;
var
  r: string;
begin
  r := '$("#' + id + '").dialog({'#10;
  if modal then
    r := r + 'modal: true'#10
  else
    r := r + 'modal: false'#10;
  if title <> '' then
    r := r + ', title: "' + title + '"'#10;
  if height <> '' then
    r := r + ',height: ' + height+#10;
  if width <> '' then
    r := r + ',width: ' +  width+#10;
  r := r +  '});'#10 +
        //GetColumns + GetAutoWidth + getShrinkToFit +
        //GetDatatype + GetUrl + getEditUrl+ GetHeightJs +
        //GettreeGrid + GetGridView + getExpandColumn +
        //GetJasonReader + getEvents + getPager +
        //'});'
        #10;

   result:= r +  inherited getJsReady;
end;

function TjqDialog.html: string;
begin
  Result:=inherited html;
end;

function TjqDialog.setTitle(aTitle: string): TjqDialog;
begin
  title := aTitle;
  result := self;
end;

function TjqDialog.setModal(aModal: Boolean): TjqDialog;
begin
  Modal := aModal;
  result := self;
end;

function TjqDialog.setHeight(aHeight: integer): TjqDialog;
begin
  Height := IntToStr( aHeight);
  result := self;
end;

function TjqDialog.setWidth(aWidth: integer): TjqDialog;
begin
  Width := IntToStr( aWidth);
  result := self;
end;

constructor TjqDialog.create(aParent: TElement; aId: string);
begin
  inherited create( aParent);
  fid := aId;
  Modal := True;
  height := '0';
  width := '0';
end;

{ TjqGrid }

function TjqGrid.GetJsReady: string;
var
  r: string;

  function GetColumns: string;
  var
    Separator, Temp: string;
    i:integer;
    col: TColumn;
  begin
    Temp:= 'colNames:[';
    for i:= 0 to FColumns.Count -1 do
    begin
      if i = FColumns.Count -1 then
        Separator:= ''
      else
        Separator:= ',';
      Temp:= Temp + '''' + TColumn(FColumns.Items[i]).Title + '''' + Separator;
    end;
    Temp:= Temp + '],'#10 +

    'colModel :['#10;
    for i:= 0 to FColumns.Count -1 do
    begin
      col := TColumn(FColumns.Items[i]);
      if i = FColumns.Count -1 then
        Separator:= ''
      else
        Separator:= ',';
      Temp := Temp + '{name:''' + TColumn(FColumns.Items[i]).Name + ''', index:''' + TColumn(FColumns.Items[i]).Name + '''';
      if col.Width > 0 then
        Temp:= Temp + ', width:' + inttostr(TColumn(FColumns.Items[i]).Width);
      Case col.Align of
        //caLeft: Temp:= Temp + ', align:''left''';  DEFAULT
        caRight: Temp:= Temp + ', align:''right''';
        caCenter: Temp:= Temp + ', align:''center''';
      end;
      if not col.Sortable then
        Temp := Temp + ', sortable:false';
      if col.hidden then
        Temp := Temp + ', hidden:true';
      if col.editable then
        Temp := Temp + ', editable:true';
      if col.key then
        Temp := Temp + ', key:true';
      if col.sorttype <> '' then
        Temp := Temp + ', sorttype: "'+ col.sorttype +'"'#10;


      Temp:= Temp + '}' + Separator + #10;
    end;
    Temp := Temp + ']'#10;

    result := temp;
  end;

begin
  r := '$("#' + id + '").jqGrid({'#10 +
       GetColumns + GetAutoWidth + getShrinkToFit +
       GetDatatype + GetUrl + getEditUrl+ GetHeightJs +
       GettreeGrid + GetGridView + getExpandColumn +
       GetJasonReader + getEvents + getPager +
       '});'#10;

  result:= r +  inherited getJsReady;
end;

function TjqGrid.GetDatatype: string;
begin
  if DataType = dtJson then
    result := ',datatype: "json"'#10
  else
    result := ',datatype: "xml"'#10;
end;

function TjqGrid.GetUrl: string;
begin
  if url = '' then
    result := ''
  else
    result := ',url:"'+url+'"'#10;
end;

function TjqGrid.getEditUrl: string;
begin
  if EditUrl = '' then
    result := ''
  else
    result := ',editurl:"'+EditUrl+'"'#10;

end;

function TjqGrid.GetHeightJs: string;
begin
  if fHeightJs <> '' then
    result := ',height:"'+ fHeightJs + '"'#10;
end;

function TjqGrid.GetGridView: string;
begin
  if not treeGrid then
    result := ', gridview:true'#10;
end;

function TjqGrid.getShrinkToFit: string;
begin
  if shrinkToFit then
    result := ',shrinkToFit:true'#10
  else
    result := '';
end;

function TjqGrid.GettreeGrid: string;
begin
  if treeGrid then
    begin
      result := ',treeGrid:true'#10 + ',treeGridModel:';
      if treeGridModel = tgAdjacency then
        result := result + '''adjacency'''#10
      else
        result := result + '''nested'''#10;
    end;
end;

function TjqGrid.GetJasonReader: string;
var
  s: string;
begin
  s := '';
  if DataType = dtJson then
  begin
    s := ',jsonReader: {'#10 +
    'rows:"' + jsonReader.rows    + '"'#10;
    if jsonReader.root <> '' then
      s := s + ',root:"'  + jsonReader.root    + '"'#10;

    s := s +
    ',page:"' + jsonReader.page    + '"'#10 +
    ',cell:"' + jsonReader.cell    + '"'#10 +
    ',id:"'   + jsonReader.id      + '"'#10 +
    ',records:"' + jsonReader.records + '"'#10 +
    ',total:"'+ jsonReader.total   + '"'#10;
    if  jsonReader.repeatitems then
      s := s + ',repeatitems:true'
    else
      s := s + ',repeatitems:false';
    s := s + '}'#10;
  end;
  result := s;
end;

function TjqGrid.GetAutoWidth: string;
begin
  if autowidth then
    result := ', autowidth: true'
  else
    result := '';
end;

function TjqGrid.getExpandColumn: string;
begin
  if ExpandColumn <> '' then
    result := ',ExpandColumn:"'+ ExpandColumn +'"'#10
  else
    result := '';
end;

function TjqGrid.getEvents: string;
begin
  result := '';

  if onSelectRow <> '' then
    result := ',onSelectRow: '+ onSelectRow;
  if onLoadComplete <> '' then
      result := result + ',loadComplete: '+ onLoadComplete;


end;

function TjqGrid.getPager: string;
begin
  if HasPager then
    result := ', pager : "#pager_'+fId+'"'
  else
    result := '';
end;

function TjqGrid.GetCustomContent: string;
var
  s: string;
begin
  s := '';
  if HasPager then
    s :=  '<div id="pager_'+fId+'"></div>';
  result := s;
end;

function TjqGrid.AddColumn: TColumn;
Var
  NewCol: TColumn;
  i: integer;
begin
  NewCol:= TColumn.Create;
  i:= FColumns.Add(NewCol);
  result:= TColumn(FColumns.Items[i]);  end;

procedure TjqGrid.AddColumn(ATitle: string; AName: string; AWidth: integer;
  AALign: TColAlign; AIsSortable: boolean);
Var
  NewCol: TColumn;
begin
  NewCol:= TColumn.Create;
  NewCol.Title:= ATitle;
  NewCol.Name:= AName;
  NewCol.Width:= AWidth;
  NewCol.Align:= AAlign;
  NewCol.Sortable:= AIsSortable;
  FColumns.Add(NewCol);
end;

constructor TjqGrid.create(aParent: TElement; aId: string);
begin
  inherited Create( aParent);
  tag := 'table';
  fId := aId;
  FColumns:= TObjectList.create(true);
  treeGrid:=false;
  treeGridModel := tgAdjacency;
  jsonReader.cell:='cell';
  jsonReader.id:='id';
  jsonReader.page:='page';
  jsonReader.records:='records';
  jsonReader.root:='';
  jsonReader.total:='total';
  jsonReader.rows:='rows';
  jsonReader.repeatitems:= true;
  autowidth:=false;
  hasPager := True;
end;

destructor TjqGrid.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

{ TjqTabPanel }

function TjqTabPanel.GetLink: string;
begin
  if AjaxURL <> '' then
    result := '<li><a href="'+ AjaxURL + '"><span>' + Caption + '</span></a></li>'#10
  else
    result := '<li><a href="#'+ id + '"><span>' + Caption + '</span></a></li>'#10;
end;

function TjqTabPanel.html: string;
begin
  if AjaxURL <> '' then
    result := ''
  else
   Result := inherited html;
end;

constructor TjqTabPanel.create(aParent: TElement; aId: string);
begin
  inherited create( aParent);
  fid := aId;
end;

{ TjqTabs }

function TjqTabs.GetCustomContent: string;
var
  i: integer;
  s: string;
begin
  s := '<ul>'#10;
  for i := 0 to TabList.Count - 1 do
    s := s + TjqTabPanel( TabList[i]).GetLink;

  s := s + '</ul>'#10;

  if parent <> nil then
  begin
    //showmessage(parent.ClassName);
    if parent.ClassName = 'TjqPane...' then
      s := s + '<!-- the tab-panel-wrapper becomes an inner-center pane -->'#10 +
         '<DIV class="ui-layout-content">'#10;
  end;


  result := s;
end;

function TjqTabs.GetCustomContentAfterChildren: string;
begin
  result := '';
  if parent <> nil then
    if parent.ClassName = 'TjqPane...' then
    begin
      result := '</DIV>'#10;
    end;
end;

function TjqTabs.GetJsready: string;
var
  r: string;
begin
  if not mergeIntoParent then
    r := '$("#' + id + '")'
  else
  begin
    if parent.tag = 'body' then
      r := '$("body")'
    else
      r := '$("#' + parent.id + '")';
  end;

  r := r + '.tabs('#10 + GetHeightStyleJS;

  if FIsCollapsible then
    r := r + ',{ collapsible: true }'#10;

  if FIsCollapsible then
    r := r +',{ event: "mouseover" }'#10;

  if onBeforeLoad <> '' then
    r := r + ',{beforeLoad:' + onBeforeLoad + '}'#10;

  r := r +');'#10;
  result:= r +  inherited getJsReady;
end;

function TjqTabs.GetHeightStyleJS: string;
var
    s: string;
begin
  if HeightStyle = hsAuto then
    s := 'auto'
  else if HeightStyle = hsContent then
    s:= 'content'
  else
    s := 'fill';
  result := '{ heightStyle: "'+s+'" }'#10;
end;

function TjqTabs.GetCustomCss: string;
var
  s: string;
begin
  s := inherited GetCustomCss;
  if LinkFontSize <> '' then
  begin
    if fid<>'' then
      s := s + '#' + fid +' ';
    s := s +  'li.ui-state-default {font-size : '+LinkFontSize+';};'#10;
  end;

  result := s;
end;




function TjqTabs.AddTab(aCaption: string; aId: string): TjqTabPanel;
begin
  result := TjqTabPanel.Create( self, aId);
  result.TabIndex := TabList.Add( result);
  result.Caption := aCaption;
end;

constructor TjqTabs.create(aParent: TElement; aId: string);
begin
  inherited create( aParent);
  fid := aId;
  TabList := TList.Create;
  HeightStyle := hsFill;
end;

destructor TjqTabs.destroy;
begin
  TabList.Clear;
  TabList.Free;
  inherited destroy;
end;

{ TJqLayout }

function TJqLayout.GetJsReady: string;
var
  r:string;

  function CenterTabRefresh: string;
  var
    i: integer;
    el: TElement;
  begin
    result := '';
    for i := 0 to center.children.count-1 do
    begin
      el := TElement( center.children[i]);
      if el.ClassName = 'TjqTabs' then
      begin
        result := '$( "#'+ el.id+'" ).tabs( "refresh" );';
        break;
      end;
    end;
  end;
  function NorthTabRefresh: string;
  var
    i: integer;
    el: TElement;
  begin
    result := '';
    for i := 0 to north.children.count-1 do
    begin
      el := TElement( north.children[i]);
      if el.ClassName = 'TjqTabs' then
      begin
        result := '$( "#'+ el.id+'" ).tabs( "refresh" );';
        break;
      end;
    end;
  end;
  function SouthTabRefresh: string;
  var
    i: integer;
    el: TElement;
  begin
    result := '';
    for i := 0 to south.children.count-1 do
    begin
      el := TElement( south.children[i]);
      if el.ClassName = 'TjqTabs' then
      begin
        result := '$( "#'+ el.id+'" ).tabs( "refresh" );';
        break;
      end;
    end;
  end;
  function EastTabRefresh: string;
  var
    i: integer;
    el: TElement;
  begin
    result := '';
    for i := 0 to east.children.count-1 do
    begin
      el := TElement( east.children[i]);
      if el.ClassName = 'TjqTabs' then
      begin
        result := '$( "#'+ el.id+'" ).tabs( "refresh" );';
        break;
      end;
    end;
  end;
  function WestTabRefresh: string;
  var
    i: integer;
    el: TElement;
  begin
    result := '';
    for i := 0 to west.children.count-1 do
    begin
      el := TElement( west.children[i]);
      if el.ClassName = 'TjqTabs' then
      begin
        result := '$( "#'+ el.id+'" ).tabs( "refresh" );';
        break;
      end;
    end;
  end;

begin

  if not mergeIntoParent then
    r := '$("#' + id + '")'
  else
  begin
    if parent.tag = 'body' then
      r := '$("body")'
    else
      r := '$("#' + parent.id + '")';
  end;

  r := r + '.layout({applyDefaultStyles: true'#10+
            north.GetOptionsJs + #10 +
            south.GetOptionsJs + #10 +
            center.GetOptionsJs + #10 +
            east.GetOptionsJs + #10 +
            west.GetOptionsJs + #10 +
            ',center__onresize: function(){'+
            CenterTabRefresh +
            NorthTabRefresh +
            SouthTabRefresh +
            EastTabRefresh +
            WestTabRefresh +
            '}'#10+
            '});'#10;

  result := r +  inherited getJsReady;
end;

constructor TJqLayout.create(aParent: TElement; aId: string);
begin
  inherited create(aParent);
  id := aID;
  north := TJQPane.Create(self, 'north');
  south := TJQPane.Create(self, 'south');
  west := TJQPane.Create(self, 'west');
  east := TJQPane.Create(self, 'east');
  center := TJQPane.Create(self, 'center');
end;


{ TjqPane }

function TjqPane.GetId: string;
begin
  if fid <> '' then
    result := fid
  else
    if Parent.Id <> '' then
      Result := Parent.Id + '_'+kind
    else
      Result := '';
end;

function TjqPane.GetCustomClasses: string;
begin
  result := 'ui-layout-'+Kind;
end;

function TjqPane.GetOptionsJs: string;
begin
  Result:='';
  if not resizable then
    result := result + ', '+ kind+'__resizable: false '#10;

  if not closable then
    result := result + ', '+ kind+'__closable: false '#10;

  if spacingOpen <> 6 then
    result := result + ', '+ kind+'__spacing_open:' + IntToStr(spacingOpen) +#10;

  if spacingClosed <> 6 then
    result := result + ', '+ kind+'__spacing_close:' + IntToStr(spacingclosed) +#10;

  if initClosed then
    result := result + ', '+ kind+'__initClosed: true'#10;

  if size <> '' then
    result := result + ', '+ kind+'__size:"'+size+'"'#10;
end;

constructor TjqPane.create(aParent: TElement; akind: string);
begin
  inherited create( aParent);
  resizable:=true;
  closable:=true;
  kind:=akind;
  spacingOpen := 6;
  spacingClosed := 6;
  initClosed := False;
  enabled := True;

end;

{ TColumn }

constructor TColumn.create;
begin
  Align := caLeft;
  hidden := false;
  editable := false;
end;
end.

