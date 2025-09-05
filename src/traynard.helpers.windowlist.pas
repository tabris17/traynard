unit Traynard.Helpers.WindowList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Windows,
  Traynard.Window, Traynard.Types, Traynard.Strings, Traynard.Helpers;

type

  { TWindowListViewHelper }

  TWindowListViewHelper = class helper for TListView
    function GetSelectedWindow(WindowCollection: TWindowCollection): TWindow;
    procedure UpdateWindowList(WindowCollection: TWindowCollection; IconList: TImageList);
  end;

implementation

{ TWindowListViewHelper }

function TWindowListViewHelper.GetSelectedWindow(WindowCollection: TWindowCollection): TWindow;
var
  SelectedItem: TListItem;
begin
  SelectedItem := Self.Selected;
  if not Assigned(SelectedItem) or not WindowCollection.Find(HWND(SelectedItem.Data), Result) then
    raise ERuntimeError.Create(ERROR_NO_SELECTED_WINDOW);
end;

procedure TWindowListViewHelper.UpdateWindowList(WindowCollection: TWindowCollection; IconList: TImageList);
var
  Window: TWindow;
  SelectedWindow: HWND = 0;
begin
  if SelCount > 0 then SelectedWindow := HWND(Selected.Data);
  BeginUpdate;
  Clear;
  IconList.Reset(1);
  for Window in WindowCollection do
  begin
    with Items.Add do
    begin
      Data := Pointer(Window.Handle);
      if SelectedWindow = Window.Handle then Selected := True;
      Caption := Window.Text;
      if Window is TTrayWindow then
      begin
        case (Window as TTrayWindow).Position of
          tpIcon: SubItems.Add(TEXT_ICON);
          tpMenu: SubItems.Add(TEXT_MENU);
        else
          SubItems.Add('N/A');
        end;
      end;
      SubItems.Add(IntToStr(Window.Handle));
      SubItems.Add(Window.ClassName);
      SubItems.Add(IntToStr(Window.PID));
      SubItems.Add(Window.AppPath);
      ImageIndex := IconList.AddIcon(Window.Icon, 0);
    end;
  end;
  EndUpdate;
end;

end.

