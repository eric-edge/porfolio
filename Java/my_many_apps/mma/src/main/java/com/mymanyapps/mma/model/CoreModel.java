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
package com.mymanyapps.mma.model;

import java.util.Iterator;

public class CoreModel
    extends AbstractModel
    implements Iterator<Long>
{
    public static final int CORE_SIZE = 3;

    private Long lastID;
    private PaneModel[][] coreModels = new PaneModel[1][CORE_SIZE];

    public CoreModel()
    {
        this.lastID = 0L;

        buildCorePaneApps();
    }

    private void buildCorePaneApps()
    {
        coreModels[0][0] = new PaneModel(PaneAppType.TOC);
        coreModels[0][1] = new PaneModel(PaneAppType.CORE);
        coreModels[0][2] = new PaneModel(PaneAppType.APP_STATUS);
    }

    @Override
    public PaneModel get(int x, int y)
    {
        return coreModels[x][y];
    }

    @Override
    public int getSize()
    {
        return CORE_SIZE;
    }

    @Override
    public boolean hasNext()
    {
        return Long.MAX_VALUE > lastID;
    }

    @Override
    synchronized public Long next()
    {
        lastID += 1;

        return lastID;
    }
}
