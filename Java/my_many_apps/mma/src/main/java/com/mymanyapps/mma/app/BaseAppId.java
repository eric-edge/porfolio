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
package com.mymanyapps.mma.app;

import com.google.common.base.Objects;

public class BaseAppId
{
    public enum Category
    {
        CORE,
        USER;
    }

    private final Category category;
    private final Integer x;
    private final Integer y;
    private final String name;

    public BaseAppId(Category category, Integer x, Integer y, String name)
    {
        this.category = category;
        this.x = x;
        this.y = y;
        this.name = name;
    }

    public Category getCategory()
    {
        return category;
    }

    public Integer getX()
    {
        return x;
    }

    public Integer getY()
    {
        return y;
    }

    public String getName()
    {
        return name;
    }

    public boolean match(BaseAppId appId)
    {
        if (appId.category != null && !appId.category.equals(category))
        {
            return false;
        }

        if (appId.x != null && appId.x != x)
        {
            return false;
        }

        if (appId.y != null && appId.y != y)
        {
            return false;
        }

        if (appId.name != null && !appId.name.equals(name))
        {
            return false;
        }

        return true;
    }

    @Override
    public boolean equals(Object other)
    {
        if (other == null)
        {
            return false;
        }

        if (this == other)
        {
            return true;
        }

        if (!(other instanceof BaseAppId))
        {
            return false;
        }

        BaseAppId appId = (BaseAppId) other;

        boolean equal =
            Objects.equal(this.category, appId.category) &&
            Objects.equal(this.x, appId.x) &&
            Objects.equal(this.y, appId.y) &&
            Objects.equal(this.name, appId.name);

        return equal;
    }
}
