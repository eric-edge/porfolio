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

import java.util.Observable;

import com.mymanyapps.mma.app.AbstractAppStatus;
import com.mymanyapps.mma.app.AppStatusApp;

import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public class AppStatusAppView
    extends AbstractAppView<AppStatusApp>
{
    private final ObservableList<String> statusMessages;

    public AppStatusAppView(AppStatusApp paneApp)
    {
        super(paneApp);

        statusMessages = FXCollections.observableArrayList();

        initComponents();
    }

    @Override
    protected void initComponents()
    {
        super.initComponents();

        TableView<String> tableView = new TableView<>();
        tableView.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);

        TableColumn<String, String> col1 = new TableColumn<>("Apps status changes");
        tableView.getColumns().add(col1);
        col1.setCellValueFactory(data -> new SimpleStringProperty(data.getValue()));

        tableView.setItems(statusMessages);

        setCenter(tableView);
    }

    public void update(
        Observable o,
        Object arg)
    {
        if (o == null)
        {
            return;
        }

        if (o instanceof AbstractAppStatus)
        {
            AbstractAppStatus status = (AbstractAppStatus) o;
            statusMessages.add(0, status.textual(null));
        }
    }
}
