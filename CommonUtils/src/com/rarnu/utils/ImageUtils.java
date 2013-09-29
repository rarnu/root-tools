package com.rarnu.utils;

import android.content.Context;
import android.graphics.*;
import android.graphics.Bitmap.CompressFormat;
import android.graphics.Bitmap.Config;
import android.graphics.PorterDuff.Mode;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import java.io.*;

public class ImageUtils {

    public static Drawable loadActionBarIcon(Context context, int res) {
        Drawable d = null;
        try {
            Bitmap bmp = BitmapFactory.decodeResource(context.getResources(),
                    res);
            d = new BitmapDrawable(bmp);
            d = resizeActionIcon(d);
        } catch (Exception e) {

        }
        return d;
    }

    private static Drawable resizeActionIcon(Drawable drawable) {
        int heightBase = UIUtils.dipToPx(24);
        int height = (int) (heightBase * UIUtils.getDensity());
        Drawable d = ImageUtils.zoomDrawable(drawable, height, height);
        return d;
    }

    public static Bitmap getBitmapByAssets(Context context, String path) {
        Bitmap bitmap = null;
        InputStream in = null;
        try {
            in = context.getResources().getAssets().open(path);
            bitmap = BitmapFactory.decodeStream(in);
        } catch (Exception e) {

        } finally {
            try {
                in.close();
            } catch (Exception ex) {

            }
        }
        return bitmap;
    }

    public static Bitmap roundedCornerBitmap(Bitmap bitmap, float radis) {
        Bitmap output = Bitmap.createBitmap(bitmap.getWidth(), bitmap.getHeight(), Config.ARGB_8888);
        Canvas canvas = new Canvas(output);

        // final int color = 0xff424242;
        final Paint paint = new Paint();
        final Rect rect = new Rect(0, 0, bitmap.getWidth(), bitmap.getHeight());
        final RectF rectF = new RectF(rect);
        final float roundPx = radis;

        paint.setAntiAlias(true);
        canvas.drawARGB(0, 0, 0, 0);
        paint.setColor(Color.WHITE);
        canvas.drawRoundRect(rectF, roundPx, roundPx, paint);

        paint.setXfermode(new PorterDuffXfermode(Mode.SRC_IN));
        canvas.drawBitmap(bitmap, rect, rect, paint);
        return output;
    }

    public static Bitmap blackWhiteBmp(Bitmap bmp) {
        // black and white

        int width = bmp.getWidth();
        int height = bmp.getHeight();

        ColorMatrix matrix = new ColorMatrix();
        float[] src = {
                0.308F, 0.609F, 0.082F, 0, 0,
                0.308F, 0.609F, 0.082F, 0, 0,
                0.308F, 0.609F, 0.082F, 0, 0,
                0, 0, 0, 1, 0
        };
        matrix.set(src);
        ColorMatrixColorFilter filter = new ColorMatrixColorFilter(matrix);
        Paint p = new Paint();
        p.setColorFilter(filter);
        Bitmap colorBmp = Bitmap.createBitmap(width, height, bmp.getConfig());
        Canvas c = new Canvas(colorBmp);
        c.drawBitmap(bmp, 0, 0, p);
        return colorBmp;
    }

    public static Bitmap rotateBmp(Bitmap bmp, int angle) {

        int width = bmp.getWidth();
        int height = bmp.getHeight();
        Matrix matrix = new Matrix();
        matrix.postRotate(angle);
        return Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true);

    }

    public static Bitmap flipBmp(Bitmap bmp, int mode) {
        // mode 1: h, 2:v

        int width = bmp.getWidth();
        int height = bmp.getHeight();
        Matrix matrix = new Matrix();
        Matrix temp = new Matrix();
        float[] mirrorY = {-1, 0, 0, 0, 1, 0, 0, 0, 1};
        temp.setValues(mirrorY);
        matrix.postConcat(temp);
        if (mode == 2) {
            matrix.setRotate(180, width / 2, height / 2);
        }
        return Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true);
    }

    public static Bitmap matrixBmp(Bitmap bmp, int margin) {

        int width = bmp.getWidth();
        int height = bmp.getHeight();
        int px = UIUtils.dipToPx(margin);
        Matrix matrix = new Matrix();
        if (width > UIUtils.getWidth() - px) {
            float scale = ((float) (UIUtils.getWidth() - px) / width);
            matrix.postScale(scale, scale);
        } else {
            matrix.postScale(1, 1);
        }
        return Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true);
    }

    public static byte[] readFileImage(String filename) throws Exception {
        BufferedInputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(filename));
        int len = bufferedInputStream.available();
        byte[] bytes = new byte[len];
        int r = bufferedInputStream.read(bytes);
        if (len != r) {
            bytes = null;
        }
        bufferedInputStream.close();
        return bytes;
    }

    public static void saveBitmapToFile(Bitmap bmp, String fileName, CompressFormat format) {
        try {
            File f = new File(fileName);
            f.createNewFile();
            FileOutputStream fOut = null;
            fOut = new FileOutputStream(f);
            bmp.compress(format, 100, fOut);
            fOut.flush();
            fOut.close();
        } catch (Exception e) {

        }

    }

    public static Bitmap blurBmp(Bitmap bmp, int Blur) {

        int pixels[] = new int[bmp.getWidth() * bmp.getHeight()];
        int pixelsRawSource[] = new int[bmp.getWidth() * bmp.getHeight() * 3];
        int pixelsRawNew[] = new int[bmp.getWidth() * bmp.getHeight() * 3];
        bmp.getPixels(pixels, 0, bmp.getWidth(), 0, 0, bmp.getWidth(), bmp.getHeight());

        for (int k = 1; k <= Blur; k++) {

            for (int i = 0; i < pixels.length; i++) {
                pixelsRawSource[i * 3 + 0] = Color.red(pixels[i]);
                pixelsRawSource[i * 3 + 1] = Color.green(pixels[i]);
                pixelsRawSource[i * 3 + 2] = Color.blue(pixels[i]);
            }

            int CurrentPixel = bmp.getWidth() * 3 + 3;
            for (int i = 0; i < bmp.getHeight() - 3; i++) {
                for (int j = 0; j < bmp.getWidth() * 3; j++) {
                    CurrentPixel += 1;

                    int sumColor = 0;
                    sumColor = pixelsRawSource[CurrentPixel - bmp.getWidth()
                            * 3];
                    sumColor = sumColor + pixelsRawSource[CurrentPixel - 3];
                    sumColor = sumColor + pixelsRawSource[CurrentPixel + 3];
                    sumColor = sumColor + pixelsRawSource[CurrentPixel + bmp.getWidth() * 3];
                    pixelsRawNew[CurrentPixel] = Math.round(sumColor / 4);
                }
            }

            for (int i = 0; i < pixels.length; i++) {
                pixels[i] = Color.rgb(pixelsRawNew[i * 3 + 0], pixelsRawNew[i * 3 + 1], pixelsRawNew[i * 3 + 2]);
            }
        }

        Bitmap bmpReturn = Bitmap.createBitmap(bmp.getWidth(), bmp.getHeight(), Config.ARGB_8888);
        bmpReturn.setPixels(pixels, 0, bmp.getWidth(), 0, 0, bmp.getWidth(), bmp.getHeight());

        return bmpReturn;

    }

    public static Bitmap colorMatrixBmp(Bitmap bmp, float[] matrixSrc) {

        int width = bmp.getWidth();
        int height = bmp.getHeight();

        ColorMatrix matrix = new ColorMatrix();
        matrix.set(matrixSrc);
        ColorMatrixColorFilter filter = new ColorMatrixColorFilter(matrix);
        Paint p = new Paint();
        p.setColorFilter(filter);
        Bitmap colorBmp = Bitmap.createBitmap(width, height, bmp.getConfig());
        Canvas c = new Canvas(colorBmp);
        c.drawBitmap(bmp, 0, 0, p);
        return colorBmp;
    }

    public static Bitmap zoomImage(Bitmap bmp, double newWidth, double newHeight) {
        float width = bmp.getWidth();
        float height = bmp.getHeight();
        Matrix matrix = new Matrix();
        float scaleWidth = ((float) newWidth) / width;
        float scaleHeight = ((float) newHeight) / height;
        matrix.postScale(scaleWidth, scaleHeight);
        Bitmap bitmap = Bitmap.createBitmap(bmp, 0, 0, (int) width, (int) height, matrix, true);
        return bitmap;
    }

    public static Bitmap drawableToBitmap(Drawable drawable) {

        int w = drawable.getIntrinsicWidth();
        int h = drawable.getIntrinsicHeight();

        Bitmap.Config config = drawable.getOpacity() != PixelFormat.OPAQUE ? Bitmap.Config.ARGB_8888 : Bitmap.Config.RGB_565;

        Bitmap bitmap = Bitmap.createBitmap(w, h, config);
        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, w, h);
        drawable.draw(canvas);
        return bitmap;
    }

    public static Drawable zoomDrawable(Drawable drawable, int w, int h) {
        int width = drawable.getIntrinsicWidth();
        int height = drawable.getIntrinsicHeight();
        Bitmap oldbmp = drawableToBitmap(drawable);
        Matrix matrix = new Matrix();
        float sx = ((float) w / width);
        float sy = ((float) h / height);
        matrix.postScale(sx, sy);
        Bitmap newbmp = Bitmap.createBitmap(oldbmp, 0, 0, width, height, matrix, true);
        return new BitmapDrawable(newbmp);
    }

}
