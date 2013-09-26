package com.rarnu.devlib.component.tools;

import android.graphics.Bitmap;
import android.opengl.GLUtils;

import javax.microedition.khronos.opengles.GL10;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

public class Texture {

    private FlipRenderer renderer;
    private int[] id = {0};
    private int width, height;
    private int contentWidth, contentHeight;
    private boolean destroyed = false;

    private Texture() {
    }

    public static Texture createTexture(Bitmap bitmap, FlipRenderer renderer, GL10 gl) {
        Texture t = new Texture();
        t.renderer = renderer;
        int potW = Integer.highestOneBit(bitmap.getWidth() - 1) << 1;
        int potH = Integer.highestOneBit(bitmap.getHeight() - 1) << 1;

        t.contentWidth = bitmap.getWidth();
        t.contentHeight = bitmap.getHeight();
        t.width = potW;
        t.height = potH;

        gl.glGenTextures(1, t.id, 0);
        gl.glBindTexture(GL10.GL_TEXTURE_2D, t.id[0]);

        gl.glTexParameterf(GL10.GL_TEXTURE_2D, GL10.GL_TEXTURE_MIN_FILTER, GL10.GL_LINEAR);
        gl.glTexParameterf(GL10.GL_TEXTURE_2D, GL10.GL_TEXTURE_MAG_FILTER, GL10.GL_LINEAR);
        gl.glTexParameterf(GL10.GL_TEXTURE_2D, GL10.GL_TEXTURE_WRAP_S, GL10.GL_CLAMP_TO_EDGE);
        gl.glTexParameterf(GL10.GL_TEXTURE_2D, GL10.GL_TEXTURE_WRAP_T, GL10.GL_CLAMP_TO_EDGE);

        switch (bitmap.getConfig()) {
            case ARGB_8888:
                gl.glTexImage2D(GL10.GL_TEXTURE_2D, 0, GL10.GL_RGBA, potW, potH, 0, GL10.GL_RGBA, GL10.GL_UNSIGNED_BYTE, null);
                GLUtils.texSubImage2D(GL10.GL_TEXTURE_2D, 0, 0, 0, bitmap);
                break;
            case ARGB_4444:
                gl.glTexImage2D(GL10.GL_TEXTURE_2D, 0, GL10.GL_RGBA, potW, potH, 0, GL10.GL_RGBA, GL10.GL_UNSIGNED_SHORT_4_4_4_4, null);
                GLUtils.texSubImage2D(GL10.GL_TEXTURE_2D, 0, 0, 0, bitmap);
                break;
            case RGB_565:
                gl.glTexImage2D(GL10.GL_TEXTURE_2D, 0, GL10.GL_RGB, potW, potH, 0, GL10.GL_RGB, GL10.GL_UNSIGNED_SHORT_5_6_5, null);
                GLUtils.texSubImage2D(GL10.GL_TEXTURE_2D, 0, 0, 0, bitmap);
                break;
            case ALPHA_8:
            default:
                throw new RuntimeException();
        }

        return t;
    }

    public static boolean isValidTexture(Texture t) {
        return t != null && !t.isDestroyed();
    }

    public static float d2r(float degree) {
        return degree * (float) Math.PI / 180f;
    }

    public static FloatBuffer toFloatBuffer(float[] v) {
        ByteBuffer buf = ByteBuffer.allocateDirect(v.length * 4);
        buf.order(ByteOrder.nativeOrder());
        FloatBuffer buffer = buf.asFloatBuffer();
        buffer.put(v);
        buffer.position(0);
        return buffer;
    }

    public static ShortBuffer toShortBuffer(short[] v) {
        ByteBuffer buf = ByteBuffer.allocateDirect(v.length * 2);
        buf.order(ByteOrder.nativeOrder());
        ShortBuffer buffer = buf.asShortBuffer();
        buffer.put(v);
        buffer.position(0);
        return buffer;
    }

    public void postDestroy() {
        renderer.postDestroyTexture(this);
    }

    public void destroy(GL10 gl) {
        if (id[0] != 0) {
            gl.glDeleteTextures(1, id, 0);
        }

        id[0] = 0;
        destroyed = true;
    }

    public boolean isDestroyed() {
        return destroyed;
    }

    public int[] getId() {
        return id;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public int getContentWidth() {
        return contentWidth;
    }

    public int getContentHeight() {
        return contentHeight;
    }
}
