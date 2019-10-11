package com.mymanyapps.mmd.view;

import com.mymanyapps.mma.view.AbstractAppView;
import com.mymanyapps.mmd.app.LabelPaneApp;

import javafx.scene.control.Label;

public class LabelPaneAppView
    extends AbstractAppView<LabelPaneApp>
{
    private final Label label;
    private String text;

    public LabelPaneAppView(LabelPaneApp paneApp)
    {
        this(paneApp, "");
    }

    public LabelPaneAppView(LabelPaneApp paneApp, String text)
    {
        super(paneApp);

        this.paneApp.inject(this);
        this.text = text;
        this.label = new Label(text);

        initComponents();
    }

    @Override
    protected void initComponents()
    {
        super.initComponents();

        setCenter(label);
    }

    public void setText(String text)
    {
        label.setText(text);
    }

    public String getText()
    {
        return text;
    }
}
