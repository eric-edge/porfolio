package com.mymanyapps.mmd.view;

import com.mymanyapps.mma.view.AbstractAppView;
import com.mymanyapps.mmd.app.RedisCommandApp;
import com.mymanyapps.mmd.presenter.RedisCommandPresenter;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

public class RedisCommandView
    extends AbstractAppView<RedisCommandApp>
{
    private final RedisCommandPresenter presenter;

    public RedisCommandView(RedisCommandApp paneApp)
    {
        super(paneApp);

        this.presenter = new RedisCommandPresenter(paneApp);

        initComponents();
    }

    @Override
    protected void initComponents()
    {
        super.initComponents();

        BorderPane centerPanel = buildCenterPanel();

        setCenter(centerPanel);
    }

    private BorderPane buildCenterPanel()
    {
        BorderPane panel = new BorderPane();

        HBox centerBox = new HBox();
        centerBox.setAlignment(Pos.CENTER);

        VBox leftBox = new VBox();
        leftBox.setAlignment(Pos.CENTER);
        Label leftLabel = new Label();
        leftLabel.setText("Left");
        leftBox.getChildren().add(leftLabel);
        leftBox.setPadding(new Insets(50, 50, 50, 50));
        leftBox.setStyle("-fx-background-color: #336699;");

        VBox rightBox = new VBox();
        rightBox.setAlignment(Pos.CENTER);
        Label rightLabel = new Label();
        rightLabel.setText("Right");
        rightBox.getChildren().add(rightLabel);
        rightBox.setPadding(new Insets(50, 50, 50, 50));
        rightBox.setStyle("-fx-background-color: #996633;");

        centerBox.getChildren().add(leftBox);
        centerBox.getChildren().add(rightBox);

        panel.setCenter(centerBox);

        return panel;
    }
}
