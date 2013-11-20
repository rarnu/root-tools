package com.rarnu.utils;

import android.content.Context;
import android.content.Intent;

public class FloatUtils {

    public static void showFloatWindow(Context context, Class<?> service, int x, int y) {
        Intent inServiceFloat = new Intent(context, service);
        inServiceFloat.putExtra("x", x);
        inServiceFloat.putExtra("y", y);
        context.startService(inServiceFloat);
    }

    public static void hideFloatWindow(Context context, Class<?> service) {
        Intent inServiceFloat = new Intent(context, service);
        context.stopService(inServiceFloat);
    }

}
