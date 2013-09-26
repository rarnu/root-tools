package com.rarnu.devlib.component.tools;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.os.Handler;
import android.os.Looper;
import android.view.View;

import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

public class FlipUI {

    private static Handler shared_handler = null;

    public static Handler getHandler() {
        return shared_handler;
    }

    public static boolean isMainThread() {
        return Looper.myLooper() == Looper.getMainLooper();
    }

    public static void assertInMainThread() {
        if (!isMainThread()) {
            throw new RuntimeException();
        }
    }

    public static void recycleBitmap(Bitmap bm) {
        if (bm != null) {
            if (bm.isRecycled()) {

            } else {
                bm.recycle();
            }
        }
    }

    public static <T> T callInMainThread(Callable<T> call) throws Exception {
        if (isMainThread()) {
            return call.call();
        } else {
            FutureTask<T> task = new FutureTask<T>(call);
            getHandler().post(task);
            return task.get();
        }
    }

    public static Bitmap takeScreenshot(View view, Bitmap.Config config) {
        int width = view.getWidth();
        int height = view.getHeight();

        if (view != null && width > 0 && height > 0) {
            Bitmap bitmap = Bitmap.createBitmap(width, height, config);
            Canvas canvas = new Canvas(bitmap);
            view.draw(canvas);
            return bitmap;
        } else {
            return null;
        }
    }
}
