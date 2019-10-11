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
package com.mymanyapps.mma.tools;

import javafx.geometry.HPos;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

public class StatusGridDisplay
    extends GridPane
{
    private final StatusGridDisplayPresenter presenter;

    private final int size;
    private StackPane[][] cells;

    public StatusGridDisplay(int size)
    {
        this.presenter = new StatusGridDisplayPresenter(this);

        this.size = size;
        this.cells = new StackPane[size][size];

        initComponents();
    }

    private void initComponents()
    {
        setStyle("-fx-border-color: darkgray; -fx-background-color: darkgray");

        double side = size * 25.0;
        double cellSide = side / size;
        setPrefSize(side, side);
        setHgap(2);
        setVgap(2);
        for (int col = 0; col < size; col++)
        {
            ColumnConstraints column = new ColumnConstraints();
            column.setHalignment(HPos.CENTER);
            column.setPercentWidth((1.0/size) * 100);
            getColumnConstraints().add(column);

            for (int line = 0; line < size; line++)
            {
                StackPane cell = new StackPane();
                cell.setStyle("-fx-border-color: darkgray");
                cell.getChildren().add(getRect(cellSide));
                cells[line][col] = cell;
                add(cell, col, line);
            }
        }
    }

    private Rectangle getRect(double cellSide)
    {
        Rectangle r = new Rectangle(cellSide, cellSide, Color.WHITESMOKE);

        return r;
    }
}
