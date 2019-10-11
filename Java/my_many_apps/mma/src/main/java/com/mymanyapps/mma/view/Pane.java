/*******************************************************************************
 * MIT License
 *
 * Copyright (c) 2019 Eric Thivierge
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Contributors:
 *     Eric Thivierge
 *******************************************************************************/
package com.mymanyapps.mma.view;

import java.awt.Point;
import java.util.HashMap;
import java.util.Map;

import com.google.common.base.Preconditions;
import com.mymanyapps.mma.model.AbstractPaneModel;
import com.mymanyapps.mma.model.CoreModel;
import com.mymanyapps.mma.model.PaneModel;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mma.tools.StatusGridDisplay;

import javafx.geometry.Insets;
import javafx.geometry.Side;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.StackPane;

public class Pane
    extends AbstractAppView<AbstractPaneApp<?>>
{
    private final CoreModel model;
    private final View view;
    private final int size;

    private final TabPane tabPane;
    private final BorderPane bottomPane;

    private final PaneModel[][] coreModels = new PaneModel[1][CoreModel.CORE_SIZE];
    private final AbstractPaneApp<?>[][] coreApps = new AbstractPaneApp[1][CoreModel.CORE_SIZE];
    private final AbstractPaneModel[][] userModels;
    private final AbstractPaneApp<?>[][] userApps;
    private final Map<AbstractPaneModel, Point> panesPosition = new HashMap<>();

    private TabPane[] userTabPanes;

    private boolean isBuilt = false;

    public Pane(CoreModel model, View view, int size)
    {
        super(null);

        this.model = model;
        this.view = view;
        this.size = size;
        this.userModels = new AbstractPaneModel[size][size];
        this.userApps = new AbstractPaneApp[size][size];

        this.tabPane = new TabPane();
        this.bottomPane = new BorderPane();

        initComponents();
    }

    @Override
    protected void initComponents()
    {
        super.initComponents();
        setPadding(new Insets(3));

        tabPane.setPadding(new Insets(2));
        tabPane.setStyle("-fx-border-color: black");
        setCenter(tabPane);

        StatusGridDisplay statusGrid = new StatusGridDisplay(size);
        statusGrid.setPadding(new Insets(1));

        Label centerLabel = new Label("Center");
        StackPane centerPane = new StackPane(centerLabel);
        centerPane.setStyle("-fx-border-color: darkgray");
        centerPane.setPadding(new Insets(2));
        bottomPane.setLeft(statusGrid);
        bottomPane.setCenter(centerPane);
        bottomPane.setStyle("-fx-border-color: black");

        Button quitButton = new Button("Quit");
        quitButton.setPrefSize(40, 40);
        quitButton.setOnAction(e -> onClick());
        StackPane rightPane = new StackPane(quitButton);
        bottomPane.setRight(rightPane);

        setBottom(bottomPane);
    }

    private void onClick()
    {
        System.out.println("Quit button clicked");
        view.quit();
    }

    public void addCorePane(int y, PaneModel paneModel, AbstractPaneApp<?> coreApp)
    {
        coreModels[0][y] = paneModel;
        coreApps[0][y] = coreApp;
    }

    public void addUserPane(int x, int y, AbstractPaneModel paneModel, AbstractPaneApp<?> paneApp)
    {
        panesPosition.put(paneModel, new Point(x, y));
        userModels[x][y] = paneModel;
        userApps[x][y] = paneApp;
    }

    public void build()
    {
        Preconditions.checkState(!isBuilt);

        // build core apps
        TabPane coreTabPane = new TabPane();
        coreTabPane.setSide(Side.LEFT);

        // toA
        Tab tocTab = new Tab();
        tocTab.setClosable(false);
        Label tocLabel = new Label("ToA");
        tocLabel.setRotate(-90);
        tocTab.setGraphic(tocLabel);
        tocTab.setContent(coreModels[0][0].buildView(coreApps[0][0]));
        coreTabPane.getTabs().add(tocTab);

        // core
        Tab coreServiceTab = new Tab();
        coreServiceTab.setClosable(false);
        Label coreLabel = new Label("1");
        coreLabel.setRotate(-90);
        coreServiceTab.setGraphic(coreLabel);
        coreServiceTab.setContent(coreModels[0][1].buildView(coreApps[0][1]));
        coreTabPane.getTabs().add(coreServiceTab);

        // Core services
        for (int y = 2; y < CoreModel.CORE_SIZE; y++)
        {
            Tab serviceTab = new Tab();
            serviceTab.setClosable(false);
            Label serviceLabel = new Label(Integer.toString(y));
            serviceLabel.setRotate(-90);
            serviceTab.setGraphic(serviceLabel);
            serviceTab.setContent(coreModels[0][y].buildView(coreApps[0][y]));
            coreTabPane.getTabs().add(serviceTab);
        }

        Tab coreTab = new Tab();
        coreTab.setClosable(false);
        coreTab.setText("Root");
        coreTab.setContent(coreTabPane);
        tabPane.getTabs().add(coreTab);

        // build user apps
        userTabPanes = new TabPane[size];
        Tab[] userTabs = new Tab[size];

        for (int x = 0; x < size; x++)
        {
            userTabs[x] = new Tab();
            userTabs[x].setClosable(false);
            userTabs[x].setText("ABCDEFGHIJKLMNOP".substring(x, x+1));
            userTabPanes[x] = new TabPane();
            userTabPanes[x].setSide(Side.LEFT);
            userTabs[x].setContent(userTabPanes[x]);
        }

        for (int x = 0; x < size; x++)
        {
            for (int y = 0; y < size; y++)
            {
                userTabPanes[x].getTabs().add(buildTab(x, y));
            }
        }

        for (int x = 0; x < size; x++)
        {
            tabPane.getTabs().add(userTabs[x]);
        }

        isBuilt = true;
    }

    private Tab buildTab(int x, int y)
    {
        Tab tab =  new Tab();
        tab.setClosable(false);

        AbstractPaneModel paneModel = userModels[x][y];
        Label label = new Label(Integer.toString(y+1));
        label.setRotate(-90);
        if (paneModel == null)
        {
            Tab emptyTab = buildEmptyTab();
            tab.setGraphic(label);
            return emptyTab;
        }

        AbstractAppView<?> view = paneModel.buildView(userApps[x][y]);
        userApps[x][y].init(paneModel);
        tab.setContent(view);
        tab.setGraphic(label);

        return tab;
    }

    private Tab buildEmptyTab()
    {
        Tab tab =  new Tab();
        tab.setClosable(false);

        return tab;
    }

    public void showPanel(AbstractPaneModel paneModel)
    {
        Point p = panesPosition.getOrDefault(paneModel, null);
        tabPane.getSelectionModel().select((int) p.getX() + 1);
        userTabPanes[(int) p.getX()].getSelectionModel().select((int) p.getY());
    }
}
