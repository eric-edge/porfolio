package com.mymanyapps.mmd.app;

import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mmd.view.LabelPaneAppView;

public class LabelPaneApp
    extends AbstractPaneApp<LabelPaneAppView>
{
    private static final String DEFAULT_TEXT = "";
    private String text;
    private LabelPaneAppView view;

    public LabelPaneApp()
    {
        this(DEFAULT_TEXT);
    }

    public LabelPaneApp(String text)
    {
        super();

        this.text = text;
    }

    public void setText(String text)
    {
        this.text = text;
        view.setText(this.text);
    }

    @Override
    public void inject(LabelPaneAppView view)
    {
        this.view = view;
    }
}
