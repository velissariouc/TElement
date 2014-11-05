unit buildhtmlunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button8: TButton;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mnSimplePage: TMenuItem;
    mnEasyUI1: TMenuItem;
    mn1: TMenuItem;
    Panel1: TPanel;
    Memo1: TSynMemo;
    SynHTMLSyn1: TSynHTMLSyn;
    procedure btDialogClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mn1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  testfile='c:\Program Files\Apache Software Foundation\Apache2.2\htdocs\test.html';

implementation



{$R *.lfm}

{ TForm1 }

uses
 Math,  cvElement, cvEasyUI, cvAngular, cvJQuery;


procedure TForm1.btDialogClick(Sender: TObject);
Var
  d: TjqDialog;
  e, b, p: TElement;
  l0, l1: TjqLayout;
  t1, t0: TjqTabs;
  t0a, t0b, t1a, t1b: TjqTabPanel;
  grid, leaf: TjqGrid;
  test: THtml;
  select: TSelect;
begin
  test := THtml.Create('panasol.gr: Πρωτόκολλο');
  test.head.AddContent('<meta charset="utf-8">');
  test.head.AddCssLink('/css/layout-default-latest.css');
//  test.head.AddCssLink('/css/ui-lightness/jquery-ui-1.10.1.custom.css', 'screen');
  test.head.AddCssLink('//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/themes/smoothness/jquery-ui.css', 'screen');
  test.head.AddCssLink('/css/ui.jqgrid.css', 'screen');
  test.head.AddScriptSrc('//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js');
  test.head.AddScriptSrc('//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/jquery-ui.min.js');
  test.head.AddScriptSrc('/js/jquery.layout.min.js');
  test.head.AddScriptSrc('/js/i18n/grid.locale-el.js');
  test.head.AddScriptSrc('/js/jquery.jqGrid.min.js');

  test.head.AddScriptSrc('/js/jquery.ba-resize.min.js');

  b:= test.body;
  d := TjqDialog.create( test.body, 'd1')
       .setTitle('Dialog Title')
       .setModal( true)
       .setHeight( 530)
       .setWidth( 380);
  //d.Css.display:='block';
  p := TParagraph.create(d);
  TLabelElement.Create( p, 'Number:', 'el_protocolno');
  TInputNumber.Create( p, 'el_protocolno').setValue('666');
   p := TParagraph.create(d);
  TLabelElement.Create( p, 'Date:', 'el_adate');
  TjqDatePicker.create( p, 'el_adate').setDateFormat('dd/mm/yy').setWidth('60px');
  p := TParagraph.create(d);
  Select := TSelect.create(p).setName('state');
  Select.setId('el_state').setWidth('100px');
  TOption.create( select, '1','FIAT');
  TOption.create( select, '2','HYUNDAI');
  TOption.create( select, '3','SKODA');
  TOption.create( select, '4','SUZUKI');

  TInputButton.create(d, '').setValue('Test');
  test.SaveToFile(testfile);
  memo1.Lines.Text:= test.html;
  test.free;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var
  e, b, d: TElement;
  l0, l1: TjqLayout;
  t1, t0: TjqTabs;
  t0a, t0b, t1a, t1b: TjqTabPanel;
  grid, leaf: TjqGrid;
  test: THtml ;
  procedure BuildLayout0;
  begin
    l0 := TJqLayout.create( b, 'l0');
    l0.mergeIntoParent:=true;

    l0.north.addContent(
    '<table style="width:100%;height:100%">'#10+
    '<tr>'#10+
    '<td style="width:60%;text-align:left; vertical-align: bottom; height: 20px; color:#0;font-size:26px;font-weight:bold;">Πρωτόκολλο</td>'#10+
    '<td style="text-align:right; height: 20px; vertical-align: bottom;font-size:14px;">  <a href="/protocol/logout">Αποσύνδεση</a></td>'#10+
    '<td style="text-align:left;vertical-align: bottom; font-size:14px;height: 20px;">'+'LoggedInLoginName'+'</td>'#10+
    '</tr>'#10+
    '</table>'#10);
    l0.north.resizable:=false;
    l0.north.size := 'auto';
    l0.north.spacingOpen := 0;
    l0.east.enabled:=false;
    l0.west.enabled:=false;
    l0.south.enabled:=false;
  end;

  procedure BuildTree;
  begin
    grid := TjqGrid.Create( t1a, 'grid1');
    With grid.AddColumn Do
      begin
        Title:= 'id';
        Name:= 'id';
        Width := 1;
        key := true;
        hidden := true;
      end;
      With grid.AddColumn Do
      begin
        Title:= 'Φάκελος';
        Name:= 'name';
        width := 180;
      end;
    grid.autowidth:=true;
    grid.DataType:=dtJson;
    grid.ExpandColumn:='name';
    grid.url:='/protocol/data?f=getjqtree';
    grid.height:='auto';
    grid.treeGrid:=true;
    grid.jsonReader.cell:='';
    grid.jsonReader.id := '0';
    grid.shrinkToFit := true;
    grid.onSelectRow:=
    'function(id){'#10+
    'var rowData = jQuery(this).getRowData(id);'#10+
    '$("#branchtitle").text(rowData[''name'']);'#10+
    //'jQuery("grid1").jqGrid(''restoreRow'',lastsel);'#10+
    //'jQuery("grid1").jqGrid(''editRow'',id,true);'#10+
    //'lastsel=id;'#10+
    '$("#leaf").jqGrid(''setGridParam'', { url: "/protocol/data?f=LeafList&key="+id+"&protocol=-1" });'#10+
    '$("#leaf").trigger("reloadGrid", [{page:1}]);'#10+
    '$("#protocolno").text( "");'#10+
    '$("#adate").text( "");'#10+
    '$("#receiver").text( "");'#10+
    '$("#sender").text( "");'#10+
    '$("#summary").text( "");'#10+
    '}';
    grid.Css.Add(
    '.ui-jqgrid-bdiv, #gbox_grid1'#10+
    '{'#10+
    'overflow-x : hidden !important;'#10+
    '}'#10
    );
  end;

  procedure BuildLeaf;
  var
    d: TDIV;
  begin
    leaf := TjqGrid.Create( l1.center, '');
    with leaf.AddColumn Do
    begin
      Title:= 'aa';
      Name:= 'aa';
      Width := 60;
      sorttype := 'int';
      hidden := true;
    end;
    with leaf.AddColumn Do
    begin
      Title:= 'eidos';
      Name:= 'eidos';
      width := 60;
      hidden := true;
    end;
    with leaf.AddColumn Do
    begin
      Title:= 'branchtitle';
      Name:= 'branchtitle';
      width := 60;
      hidden := true;
    end;
    with leaf.AddColumn Do
    begin
      Title:= 'Αρ. Πρωτ.';
      Name:= 'protocol';
      width := 60;
      sorttype := 'int';
    end;
    with leaf.AddColumn Do
    begin
      Title:= 'Ημ/νια';
      Name:= 'adate';
      width := 90;
      sorttype := 'date';
    end;
    with leaf.AddColumn Do
    begin
      Title:= 'Περίληψη';
      Name:= 'summary';
      editable:=true;
      width := 190;
    end;
    with leaf.AddColumn Do
    begin
      Title:= 'Από';
      Name:= 'sender';
      width := 150;
    end;
    with leaf.AddColumn Do
    begin
      Title:= 'Προς';
      Name:= 'receiver';
      width := 150;
    end;
    leaf.id := 'leaf';
    leaf.autowidth:=true;
    leaf.DataType:=dtJson;
    leaf.ExpandColumn:='name';
    leaf.url:= '/protocol/data?f=LeafList';
    leaf.editurl:='/protocol/editgrid';
    leaf.height:='250';
    leaf.jsonReader.cell:='';
    leaf.jsonReader.id := '0';
    leaf.jsonReader.root := 'root';
    leaf.jsonReader.repeatitems:=true;
    leaf.shrinkToFit := true;
    leaf.onSelectRow:=
      'function(id) {'#10+
      '//$("#branchtitle").text( $( this).jqGrid (''getCell'', id, ''protocol''));'#10+
      '$("#protocolno").text( $( this).jqGrid (''getCell'', id, ''protocol''));'#10+
      '$("#adate").text( $( this).jqGrid (''getCell'', id, ''adate''));'#10+
      '$("#eidos").text( $( this).jqGrid (''getCell'', id, ''eidos''));'#10+
      '$("#sender").text( $( this).jqGrid (''getCell'', id, ''sender''));'#10+
      '$("#receiver").text( $( this).jqGrid (''getCell'', id, ''receiver''));'#10+
      '$("#summary").text( $( this).jqGrid (''getCell'', id, ''summary''));'#10+
      '$("#branchtitle").text( $( this).jqGrid (''getCell'', id, ''branchtitle''));'#10+
      '}';
    leaf.onLoadComplete:=
      'function( data) {'#10+
      ' if (editid > -1) {  '#10+
      //'  alert("LoadComplete id ="+editid);'#10+
      '  jQuery("#leaf").setSelection (editid, true);'#10+
      '  editid=-1;'#10+
      '  }'#10+
      '}'#10;

    leaf.addJs(
    'var gotEditList = 0;'#10+
    'var editid = -1; '#10);
    leaf.addJSReady(
    '$("#leaf").jqGrid("navGrid","#pager_leaf", {'#10+
    '  edit: true,'#10+
    '  editfunc:'#10+
    '	  function(id){'#10+
    '	    if(!gotEditList) {'#10+
    '		  $.getScript("/protocol/js_editlist", function() {'#10+
    '		    gotEditList = 1;'#10+
    //'          alert("got editlist.js");'#10+
    '			aa = jQuery("#leaf").jqGrid ("getCell", id, "aa") ;'#10+
    '          editList(aa, id);'#10+
    '		    }'#10+
    '         );//getScript'#10+
    '		} else {'#10+
    '      aa = jQuery("#leaf").jqGrid ("getCell", id, "aa") ;'#10+
    '		//alert("call editlist");'#10+
    '      editList(aa, id);'#10+
    '		};'#10+
    'jQuery("#leaf").setSelection ( id, true);'+
    '	  },'#10+
    '	add: true,'#10+
    '  addfunc:'#10+
    '	  function(id){'#10+
    '	    if(!gotEditList) {'#10+
    '		  $.getScript("/protocol/js_editlist", function() {'#10+
    '		    gotEditList = 1;'#10+
    '          //alert("got editlist.js"); '#10+
    '			//aa = jQuery("#leaf").jqGrid ("getCell", id, "aa") ;'#10+
    '          addLeaf();'#10+
    '		    } '#10+
    '         );//getScript '#10+
    '		} else {'#10+
    '      //aa = jQuery("#leaf").jqGrid ("getCell", id, "aa") ;'#10+
    '		//alert("call editlist");'#10+
    '      addLeaf();'#10+
    '		};'#10+
    '	  },  '#10+
    '	del:true}'#10+
    '  );'#10

    );


    d := TDiv.Create( l1.center);
    d.AddContent(
    '<div id="protview" class="box" style=" width: 610px; margin-top:15px; border-radius: 15px;">'#10 +
    '<p><b>Φάκελος:</b> <span id="branchtitle"></span> </p>'#10+
    '<p><b>Αριθμός πρωτοκόλλου: </b> <span id="protocolno"></span></p>'#10 +
    '<p><b>Ημ/νια:</b> <span id="adate"></span>'#10 +
    '<b style="margin-left:50px;">Είδος:</b> <span id="eidos" ></span></p>'#10 +
    '<p><b>Από:</b> <span id="sender"></span></p>'#10 +
    '<p><b>Προς:</b> <span id="receiver"></span></p>'#10 +
    '<p><b>Περίληψη:</b> <span id="summary"></span></p> '#10 +
    '</div>'#10 +
    '<!--button type="button"  onclick="window.open(''/protocol/pdf''); return true;">pdf</button-->'#10);
  end;


  procedure BuildTab0;
  begin
    t0 := TjqTabs.Create( l0.center, 't0');
    t0.onBeforeLoad:=
      'function( event, ui ) {'#10 +
      'ui.jqXHR.error(function() {'#10 +
      'ui.panel.html('#10 +
      '''H σύνδεση έληξε! <a href="/protocol/logout">Συνδεθήτε ξανά</a>"'' );'#10 +
      '});}';
    t0.LinkFontSize:='12px';
    t0a := t0.AddTab('Εγγραφα', 't0-1');
    //t0a.padding:='2px';
    t0a.AddClass('container');
    t0b := t0.AddTab('Υπεύθυνοι', 't0-2');
    t0b.AjaxURL:='/protocol/html?f=ypeftab';
    {t0.onResize:=
    '$("#t1-2").resize(function() { '#10+
    '//waitForFinalEvent(function(){'#10+
    'var tab_width = $("#t1-2").width(); '#10+
    '//alert("t1-2 width= "+ tab_width); '#10+
    '$("#grid1").setGridWidth( tab_width); '#10+
    '//alert("t1-2b"); '#10+
    '//}, 500, "some unique string");'#10+
    '});'#10;  }
  end;

  procedure BuildTab1;
  var
    d: TDiv;
  begin
  t1 := TjqTabs.Create( l1.west, 't1');
  t1.onBeforeLoad:=
    'function( event, ui ) {'#10 +
    'ui.jqXHR.error(function() {'#10 +
    'ui.panel.html('#10 +
    '''H σύνδεση έληξε! <a href="/protocol/logout">Συνδεθήτε ξανά</a>"'' );'#10 +
    '});}';

  t1.LinkFontSize:='12px';
  t1a := t1.AddTab('Φάκελοι', 't1-1');
  t1a.AddClass(' container ');
  t1a.addJSReady(
  '$("#t1-1").resize(function() { '#10+
  '//waitForFinalEvent(function(){'#10+
  'var tab_width = $("#t1-1").width(); '#10+
  '//alert("t1-2 width= "+ tab_width); '#10+
  '$("#grid1").setGridWidth( tab_width); '#10+
  '//alert("t1-2b"); '#10+
  '//}, 500, "some unique string");'#10+
  '});'#10);
  d := TDiv.create( t1, 'ui-widget-footer ui-widget-header ui-corner-bottom');
  d.AddContent(
    '<button type="button"  title="εκτύπωση φακέλων" height="10px" top=0 '+
    'onclick="window.open(''/protocol/pdf''); return true;">'#10+
    '<img src="/images/print.gif" />'#10+
    '</button>'#10
  );
  t1b := t1.AddTab('Αναζήτηση', 't1-2');
  t1b.AjaxURL:='/protocol/html?f=searchtab';
  end;

  procedure BuildLayout1;
  begin
  l1 := TJqLayout.create( t0a, 'l1');
  l1.setHeight( '100%');
  l1.north.enabled:=false;
  l1.south.enabled:=false;
  l1.east.enabled:=false;
  l1.west.size:='30%';
  l1.mergeIntoParent:=true;
  //l1.west.AddClass(' container ');
  //l1.center.AddClass(' container ');;
  end;

begin
 test := THtml.Create('panasol.gr: Πρωτόκολλο');
 test.head.AddContent('<meta charset="utf-8">');
 test.head.AddCssLink('/css/layout-default-latest.css');
 //test.head.AddCssLink('/css/ui-lightness/jquery-ui-1.10.1.custom.css', 'screen');
 test.head.AddCssLink('/css/jquery-ui.min.css', 'screen');
 test.head.AddCssLink('/css/ui.jqgrid.css', 'screen');
 test.head.AddScriptSrc('/js/jquery.js');
 test.head.AddScriptSrc('/js/jquery-ui.js');
 test.head.AddScriptSrc('/js/jquery.layout-latest.js');
 test.head.AddScriptSrc('/js/i18n/grid.locale-el.js');
 test.head.AddScriptSrc('/js/jquery.jqGrid.min.js');
 test.head.AddScriptSrc('/js/jquery.layout.resizeTabLayout-1.3.js');
 test.head.AddScriptSrc('/js/jquery.ba-resize.min.js');
  b:= test.body;
  b.Css.Add('  .container { padding: 2px !important;}'#10+
               '.ui-layout-pane {padding: 2px !important;}'#10);
  b.css.add('.ui-widget {font-size : 12px;}'#10);
  BuildLayout0;
  BuildTab0;

  BuildLayout1;
  BuildTab1;

  BuildTree;
  BuildLeaf;

  //test.body.Content.Add('Hello world');
  test.SaveToFile( testfile);
  memo1.Lines.Text:= test.html;
  test.Free;

end;

procedure TForm1.Button4Click(Sender: TObject);
Var
  e, b, d: TElement;
  l, l2: TeasyuiLayout;
  t: TeasyuiTabs;
  p, p1: TeasyuiTabPanel;
  test: THtml;
begin
  test := THtml.Create('my first EasyUI page');
  TEasyUI.Create( test); //include JQuery EasyUI scripts and Css

  b := test.body;
  b.padding:='10px';

  l := TeasyuiLayout.create( b);
  l.mergeIntoParent:=true;
  l.fit := true;
  l.north.Height := '100px';
  l.south.Height := '50px';
  l.south.split:=true;
  l.east.split:=true;
  l.east.width:='180px';
  l.east.title:='east';

  l.west.title:='west';
  l.west.split:=true;
  l.west.width:='100px';
  l.center.title:='Main Title';
  l.center.iconCls := 'icon-ok';

  t := TeasyuiTabs.create( l.center);
  t.fit := true;
  p1 := TeasyuiTabPanel.Create( t);
  p1.title:='Tab1';
  p := TeasyuiTabPanel.Create( t);
  p.title:='Tab2';
  p := TeasyuiTabPanel.Create( t);
  p.title:='Tab3';

  e := TElement.Create( l.north);
  e.tag := 'h1';
  e.addContent('My First Heading');

  e := TElement.Create( l.north );
  e.tag := 'p';
  e.addContent('My first paragraph.');

  d := TDiv.Create( l.north, 'demo-info');
  d.title:='d.title';
  TDiv.Create( d, 'demo-tip icon-tip');
  TDiv.Create( d).addContent( 'The layout contains north,south,west,east and center regions.');

  l2 := TeasyuiLayout.create( p1);
  //l2.mergeIntoParent:=true;
  l2.fit := true;
  l2.north.height := '20px';
  l2.north.addContent('this is north');
  l2.south.height := '50px';
  l2.south.split:=true;
  l2.east.split:=true;
  l2.east.width:='80px';
  l2.west.width :='80px';
  l2.east.title:='east';
  l2.center.addContent('this is center');
  l2.west.title:='west';
  l2.center.title:='center';

  test.SaveToFile( testfile);
  memo1.Lines.Text:= test.html;
  test.Free;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  test: THtml;
  d: TDiv;
  e: TElement;
  f: TFormElement;
  fs: TFieldSet;
begin
  test := THtml.create('Πρωτόκολλο - Σύνδεση στην εφαρμογή');
  test.head.addMeta('author','Velissariou Costas velissariouc@gmail.com');
  d:=TDiv.create( test.body);
  d.setId('bar').setWidth('100%').setPadding('10px 0').setMargin('0')
  .setColor('#445058');
  test.body.Css.Add('#bar { background:url(../images/bar.png) repeat-x}'#10);
  test.body.margin:='0';
  e := TElement.Create( d);
  e.tag := 'h1';
  e.textAlign:='center';
  e.margin:='0';
  e.addContent('Πρωτόκολλο');
  TElement.Create( test.body)
  .setTag('p')
  .addContent('(πληκτολογήστε "test" στον χρήστη και στον κωδικό)')
  .setTextAlign('center');

  f := TFormElement.create( test.body);
  f.action:='/protocol/login';
  f.css.add(
  'form {'#10+
    '  margin-left: auto;'#10+
    '  margin-right: auto;'#10+
    '  width:300px;'#10+
    '  border:1px solid #899caa;'#10+
    '  border-radius:3px 0 3px 3px;'#10+
    '  -moz-border-radius:3px 0 3px 3px;'#10+
    '  margin-top:-1px;'#10+
    '  background:#d2e0ea;'#10+
    '  padding:6px;'#10+
  '}'#10
  );

  fs := TFieldSet.Create( f);


  TLabelElement.Create( fs, 'Ονομα χρήστη', 'LoginName');
  TInputText.Create( fs, 'LoginName');
  TLabelElement.Create( fs, 'Κωδικός', 'Password');
  TInputPassword.Create( fs, 'Password');
  TInputButton.Create( fs, 'login').setValue('OK').onclick:='formSubmit()';

  TDiv.Create( test.body)
  .addContent('<h3 id="missing" style="display:none; color:red">Παρακαλώ καταχωρήστε όνομα και κωδικό .</h3>')
  .addContent('<h3 id="invlogin" style="display:none; color:red">Λάθος όνομα η κωδικός. Παρακαλω δοκιμάστε ξανά.</h3>')
  .addContent('<h3 id="expired" style="display:none;color:orange">Η σύνδεση έληξε. Παρακαλώ συνδεθείτε ξανά</h3>')
  .setId('message').setAlign('center');

  test.body.Css.Add(


'fieldset {'#10+
    'display:block;'#10+
    'border:0;'#10+
'	background:#fff;'#10+
 '   border-radius:3px;'#10+
 '   -moz-border-radius:3px;'#10+
 '   padding:10px; '#10+
 '   margin:0; '#10+
'} '#10+
'label { '#10+
  '  display:block;'#10+
  '  float:none; '#10+
  '  margin:0 0 6px 0;'#10+
'}'#10+


'input { '#10+
 '   width:92%;'#10+
 '   border:1px solid #899caa;'#10+
 '   border-radius:3px; '#10+
 '   -moz-border-radius:3px; '#10+
 '   color:#3a454d;'#10+
 '   font-weight:bold;'#10+
 '   padding:8px 8px;'#10+
'	margin-bottom: 5px; '#10+
 '   box-shadow:inset 0px 1px 3px #bbb;'#10+
 '   -webkit-box-shadow:inset 0px 1px 3px #bbb;'#10+
 '   -moz-box-shadow:inset 0px 1px 3px #bbb;'#10+
 '   font-size:12px; '#10+
'} '#10+

'input:focus { '#10+
 '   outline:#445058 solid  thick; '#10+
'	background-color:LightBlue;'#10+
'}'#10+
'#login {'#10+
'    width:auto;'#10+
'    float:right;'#10+
'    background:#339cdf url(../images/loginbuttonbg.png) repeat-x;'#10+
'    color:#fff; '#10+
'    padding:7px 10px 8px 10px;  margin-top:10px;'#10+
'}'#10

  );

  test.SaveToFile(testfile);
  memo1.Lines.Text:= test.html;
  test.free;

  (*<fieldset>
<form action="process.php" method="post">
    <legend>Login</legend>
    <label for="username">Username:</label>
    <input type="text" name="username" id="username" />
    <br />
    <label for="password">Password:</label>
    <input type="password" name="password" id="password" />

</form>
</fieldset>


input, label {
    width:200px;
    display:block;
    float:left;
    margin-bottom:10px;
}
label {
    width:80px;
    text-align:right;
    padding-right:10px;
    margin-top:2px;
}
br {
    clear:left;
}
fieldset{
    border:1px solid black;
    border-radius:5px;
    width:350px;
    overflow:hidden;
}
legend {
    text-align:center;
    font-weight:bold;
    background-color:green;
    padding:5px;
    color:white;
    border-radius:0 0 5px 0;
    margin-bottom:5px;
}

*)
end;

procedure TForm1.Button6Click(Sender: TObject);
Var
  test: THtml;
  css: TCss;
  li: TListItem;
begin
  test := THtml.Create('My Awesome Page (Example 1-6)');

  TElement.Create( test.body)
    .setTag('h1').AddClass('loud')
    .addContent('Hi there!');
  TParagraph.Create( test.body)
    .addContent('Thanks for visiting my web page.');
  TParagraph.Create( test.body)
    .addContent('I hope you like it.');
  TUnorderedList.create( test.body)
    .AddItem('Pizza')
    .AddItem('Beer')
    .AddItem('Dogs')
    .GetItem(0).AddClass('loud')
    .setId('highlight');
    ;
  css := TCss.create('body');
  css.font.weight:='bold';
  css.font.size:='12px';
  css.font.family:='Arial';
  test.body.CssList.Add( css);

  css := TCss.create('h1 a');
  css.font.style := 'italic';
  test.body.CssList.Add( css);

  css := TCss.create('.loud');
  css.font.style := 'italic';
  test.body.CssList.Add( css);

  css := TCss.create('#highlight');
  css.Background.color := 'yellow';
  test.body.CssList.Add( css);

  test.SaveToFile(testfile);
  memo1.Lines.Text:= test.html;
  test.Free;
end;

procedure TForm1.Button7Click(Sender: TObject);
Var
  doc: THtml;
  css: TCss;
  aDiv: TDiv;
begin
  doc := THtml.Create('Titles should be short descriptions of the page');
  aDiv := TDiv.create( doc.body);
  TElement.Create( aDiv)
    .setTag('h1')//.AddClass('loud')
    .addContent('h1 tags should contain the most important information on your site');
  TParagraph.Create( aDiv)
    .addContent( 'Paragraph tags should be where most of your text content lives,'+
                 'or used as a separation between sections.');
  TUnorderedList.create( aDiv)
    .AddItem('This is a list element')
    .AddItem('This is another element, notice the bullets and indentation');
  TParagraph.Create( aDiv)
    .addContent( 'You probably noticed that I used a "div" tag to wrap these '+
                 'other tags in. I''m using it as a container.');
  TCss.create('body', doc)
      .setBackgroundColor('#e7e7e7')
      .setFontFamily('Helvetica, Arial, sans-serif')
      .setFontSize('0.75em');

  TCss.create('h1', doc)
      .setFontSize('200%');
  TCss.create('p', doc)
      .setMargin('5px 0')
      .setPadding('0 3px');
  TCss.create('.small', doc)
      .setFontSize('80%');
  TCss.create('ul', doc)
      .setListStyleType('none')
      .setFontWeight('bold');
  doc.SaveToFile(testfile);
  memo1.Lines.Text:= doc.html;
  doc.Free;
end;

procedure TForm1.Button8Click(Sender: TObject);

var
  s: TJavaScript;
  x: TJSVar;
begin
  s := TJavaScript.create;
  with s do
  begin
    x := NewVar( 'x', 4);
    NewVar( 'astring', '4');
    NewVar( 'Number', 0);
    alert( x);
  end;
  memo1.Clear;
  memo1.Text:= s.Code;
  s.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  label2.caption := TestFile;
end;

procedure TForm1.mn1Click(Sender: TObject);
var
  test: THtml;
  d: TDiv;

begin
  test := THtml.Create('my first html page');
  TAngular.create( test);
  TUIBootstrap.create( test);

  with TNavBar.create( test.body) do
  begin
    Brand:='My Project';
    AddMenu
      .AddItem('Item1')
      .AddItem('Angular','https://angularjs.org/')
  end;

  d := TDiv.create(test.body);
  d.addAttribute('ng-controller="CollapseDemoCtrl"');

  TButton.create( d, 'btn btn-default')
    .addContent('Toggle collapse')
    .addAttribute('ng-click="isCollapsed = !isCollapsed"');


  TDiv.create(d).addAttribute('collapse="isCollapsed"')
    .insert(
        TDiv.create.addContent('Hello world').addclass('well')
      );


  test.body.addJS(
    'var app = angular.module(''MyApp'', [''ui.bootstrap'']);'#10+
    'app.controller("CollapseDemoCtrl", function ($scope) { '#10+
    '  $scope.isCollapsed = false;});'
  );

  test.SaveToFile( testfile);
  memo1.Lines.Text:= test.html;
  test.Free;
end;

end.

