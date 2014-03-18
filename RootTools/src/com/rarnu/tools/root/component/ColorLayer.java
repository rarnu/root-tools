package com.rarnu.tools.root.component;

import android.content.Context;
import android.graphics.Canvas;
import android.view.View;

public class ColorLayer extends View {

    private int a, r, g, b;

    public ColorLayer(Context context) {
        super(context);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        canvas.drawARGB(a, r, g, b);
    }

    public void setColor(int a, int r, int g, int b) {
        this.a = a;
        this.r = r;
        this.g = g;
        this.b = b;
        invalidate();
    }

}
