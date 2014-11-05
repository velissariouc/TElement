TElement
========

a pascal library for generating html pages

The project BuildHtml contains some examples showing how
to use the library.
Here is a small snippet:
    pascal
var
  test: THtml;

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

  writeln( test.html);
  test.Free;
end;      
      
This is a hobby project. So don't expect a lot of commits.
