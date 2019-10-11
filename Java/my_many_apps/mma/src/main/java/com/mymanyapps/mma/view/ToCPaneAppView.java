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

import java.util.ArrayList;
import java.util.List;

import com.mymanyapps.mma.app.ToCPaneApp;
import com.mymanyapps.mma.model.AbstractPaneModel;

import javafx.geometry.HPos;
import javafx.scene.control.Label;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

public class ToCPaneAppView
    extends AbstractAppView<ToCPaneApp>
{
    private final List<AbstractPaneModel> models;
    private GridPane gridPane;

    public ToCPaneAppView(ToCPaneApp paneApp)
    {
        super(paneApp);

        models = new ArrayList<>();

        initComponents();
    }

    @Override
    protected void initComponents()
    {
        int size = paneApp.getSize();
        gridPane = new GridPane();
        gridPane.setHgap(15);
        gridPane.setVgap(15);
        for (int c = 0; c < size; c++)
        {
            ColumnConstraints column = new ColumnConstraints();
            column.setHalignment(HPos.CENTER);
            column.setPercentWidth((1.0/size) * 100);
            gridPane.getColumnConstraints().add(column);
        }

        setCenter(gridPane);
    }

    public void add(int x, int y, AbstractPaneModel model)
    {
        models.add(model);

        final Label label =  new Label();
        label.setText(model.getAppName());
        label.setOnMouseClicked(e -> gotoPanel(model));
        gridPane.add(label, x, y);
        GridPane.setVgrow(label, Priority.ALWAYS);
    }

    private void gotoPanel(AbstractPaneModel paneModel)
    {
        paneApp.gotoPanel(paneModel);
    }
}
