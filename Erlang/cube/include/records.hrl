%% face
-record(face, {fa=0}).


%% facette
-record(facette, {x, y, display=none}).


%% cufaxy
-record(cufaxy, {cu=0, fa=0, x=0, y=0}).

%% cube
-record(cube, {parent=none,
			   size=4,
			   type=soft,
			   cufaxy=#cufaxy{},
			   children=[],
			   data=[]}).
