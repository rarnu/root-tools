package com.rarnu.devlib.component.tools;

import javax.microedition.khronos.opengles.GL10;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

public class Card {

    public static final int AXIS_TOP = 0;
    public static final int AXIS_BOTTOM = 1;
    public static final int X_00 = 0;
    public static final int Y_00 = 1;
    public static final int Z_00 = 2;
    public static final int X_01 = 3;
    public static final int Y_01 = 4;
    public static final int Z_01 = 5;
    public static final int X_11 = 6;
    public static final int Y_11 = 7;
    public static final int Z_11 = 8;
    public static final int X_10 = 9;
    public static final int Y_10 = 10;
    public static final int Z_10 = 11;
    private float cardVertices[];
    private short[] indices = {0, 1, 2, 0, 2, 3};
    private FloatBuffer vertexBuffer;
    private ShortBuffer indexBuffer;
    private float textureCoordinates[];
    private FloatBuffer textureBuffer;
    private Texture texture;
    private float angle = 0f;
    private int axis = AXIS_TOP;
    private boolean orientationVertical = true;
    private boolean dirty = false;

    public Texture getTexture() {
        return texture;
    }

    public void setTexture(Texture texture) {
        this.texture = texture;
    }

    public float[] getCardVertices() {
        return cardVertices;
    }

    public void setCardVertices(float[] cardVertices) {
        this.cardVertices = cardVertices;
        this.dirty = true;
    }

    public short[] getIndices() {
        return indices;
    }

    public ShortBuffer getIndexBuffer() {
        return indexBuffer;
    }

    public void setTextureCoordinates(float[] textureCoordinates) {
        this.textureCoordinates = textureCoordinates;
        this.dirty = true;
    }

    public void setAngle(float angle) {
        this.angle = angle;
    }

    public void setAxis(int axis) {
        this.axis = axis;
    }

    public void setOrientation(boolean orientationVertical) {
        this.orientationVertical = orientationVertical;
    }

    public void draw(GL10 gl) {
        if (dirty) {
            updateVertices();
        }

        if (cardVertices == null) {
            return;
        }

        gl.glFrontFace(GL10.GL_CCW);

        gl.glEnable(GL10.GL_CULL_FACE);
        gl.glCullFace(GL10.GL_BACK);

        gl.glEnableClientState(GL10.GL_VERTEX_ARRAY);

        gl.glEnable(GL10.GL_BLEND);
        gl.glBlendFunc(GL10.GL_ONE, GL10.GL_ONE_MINUS_SRC_ALPHA);

        gl.glColor4f(1f, 1.0f, 1f, 1.0f);

        if (Texture.isValidTexture(texture)) {
            gl.glEnable(GL10.GL_TEXTURE_2D);
            gl.glEnableClientState(GL10.GL_TEXTURE_COORD_ARRAY);
            gl.glTexParameterf(GL10.GL_TEXTURE_2D, GL10.GL_TEXTURE_WRAP_S, GL10.GL_CLAMP_TO_EDGE);
            gl.glTexParameterf(GL10.GL_TEXTURE_2D, GL10.GL_TEXTURE_WRAP_T, GL10.GL_CLAMP_TO_EDGE);
            gl.glTexCoordPointer(2, GL10.GL_FLOAT, 0, textureBuffer);
            gl.glBindTexture(GL10.GL_TEXTURE_2D, texture.getId()[0]);
        }

        gl.glPushMatrix();

        if (orientationVertical) {
            if (angle > 0) {
                if (axis == AXIS_TOP) {
                    gl.glTranslatef(0, cardVertices[Y_00], 0f);
                    gl.glRotatef(-angle, 1f, 0f, 0f);
                    gl.glTranslatef(0, -cardVertices[Y_00], 0f);
                } else {
                    gl.glTranslatef(0, cardVertices[Y_11], 0f);
                    gl.glRotatef(angle, 1f, 0f, 0f);
                    gl.glTranslatef(0, -cardVertices[Y_11], 0f);
                }
            }
        } else {
            if (angle > 0) {
                if (axis == AXIS_TOP) {
                    gl.glTranslatef(cardVertices[X_00], 0, 0f);
                    gl.glRotatef(-angle, 0f, 1f, 0f);
                    gl.glTranslatef(-cardVertices[X_00], 0, 0f);
                } else {
                    gl.glTranslatef(cardVertices[X_11], 0, 0f);
                    gl.glRotatef(angle, 0f, 1f, 0f);
                    gl.glTranslatef(-cardVertices[X_11], 0, 0f);
                }
            }
        }

        gl.glVertexPointer(3, GL10.GL_FLOAT, 0, vertexBuffer);
        gl.glDrawElements(GL10.GL_TRIANGLES, indices.length, GL10.GL_UNSIGNED_SHORT, indexBuffer);

        gl.glPopMatrix();

        if (Texture.isValidTexture(texture)) {
            gl.glDisableClientState(GL10.GL_TEXTURE_COORD_ARRAY);
            gl.glDisable(GL10.GL_TEXTURE_2D);
        }

        float w, h, z;
        float[] shadowVertices;

        if (angle > 0) {
            float alpha = 1f * (90f - angle) / 90f;

            if (axis == AXIS_TOP) {
                if (orientationVertical) {
                    h = (cardVertices[Y_00] - cardVertices[Y_01]) * (1.0f - android.util.FloatMath.cos(Texture.d2r(angle)));
                    z = (cardVertices[Y_00] - cardVertices[Y_01]) * android.util.FloatMath.sin(Texture.d2r(angle));
                    shadowVertices = new float[]{cardVertices[X_00], cardVertices[Y_01] + h, z, cardVertices[X_01], cardVertices[Y_01], 0f, cardVertices[X_11], cardVertices[Y_11], 0f, cardVertices[X_10], cardVertices[Y_01] + h, z};
                } else {
                    w = (cardVertices[X_10] - cardVertices[X_00]) * (1.0f - android.util.FloatMath.cos(Texture.d2r(angle)));
                    z = (cardVertices[X_10] - cardVertices[X_00]) * android.util.FloatMath.sin(Texture.d2r(angle));
                    shadowVertices = new float[]{cardVertices[X_10] - w, cardVertices[Y_00], z, cardVertices[X_11] - w, cardVertices[Y_01], z, cardVertices[X_11], cardVertices[Y_11], 0f, cardVertices[X_10], cardVertices[Y_10], 0f};
                }
            } else {
                if (orientationVertical) {
                    h = (cardVertices[Y_00] - cardVertices[Y_01]) * (1f - android.util.FloatMath.cos(Texture.d2r(angle)));
                    z = (cardVertices[Y_00] - cardVertices[Y_01]) * android.util.FloatMath.sin(Texture.d2r(angle));
                    shadowVertices = new float[]{cardVertices[X_00], cardVertices[Y_00], 0f, cardVertices[X_01], cardVertices[Y_00] - h, z, cardVertices[X_11], cardVertices[Y_00] - h, z, cardVertices[X_10], cardVertices[Y_00], 0f};
                } else {
                    w = (cardVertices[X_10] - cardVertices[X_00]) * (1f - android.util.FloatMath.cos(Texture.d2r(angle)));
                    z = (cardVertices[X_10] - cardVertices[X_00]) * android.util.FloatMath.sin(Texture.d2r(angle));
                    shadowVertices = new float[]{cardVertices[X_00], cardVertices[Y_00], 0f, cardVertices[X_01], cardVertices[Y_01], 0f, cardVertices[X_00] + w, cardVertices[Y_11], z, cardVertices[X_01] + w, cardVertices[Y_10], z};
                }
            }

            gl.glDisable(GL10.GL_LIGHTING);
            gl.glDisable(GL10.GL_DEPTH_TEST);
            gl.glDisable(GL10.GL_TEXTURE_2D);
            gl.glEnable(GL10.GL_BLEND);
            gl.glBlendFunc(GL10.GL_ONE, GL10.GL_ONE_MINUS_SRC_ALPHA);
            gl.glColor4f(0f, 0.0f, 0f, alpha);
            gl.glVertexPointer(3, GL10.GL_FLOAT, 0, Texture.toFloatBuffer(shadowVertices));
            gl.glDrawElements(GL10.GL_TRIANGLES, indices.length, GL10.GL_UNSIGNED_SHORT, indexBuffer);

            gl.glEnable(GL10.GL_TEXTURE_2D);
            gl.glEnable(GL10.GL_DEPTH_TEST);
            gl.glEnable(GL10.GL_LIGHTING);
        }

        gl.glDisable(GL10.GL_BLEND);
        gl.glDisableClientState(GL10.GL_VERTEX_ARRAY);
        gl.glDisable(GL10.GL_CULL_FACE);
    }

    private void updateVertices() {
        vertexBuffer = Texture.toFloatBuffer(cardVertices);
        indexBuffer = Texture.toShortBuffer(indices);
        textureBuffer = Texture.toFloatBuffer(textureCoordinates);
    }
}
