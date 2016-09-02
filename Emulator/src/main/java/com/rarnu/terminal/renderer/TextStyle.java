package com.rarnu.terminal.renderer;

public class TextStyle {

    public final static int fxNormal = 0;
    public final static int fxBold = 1;
    public final static int fxItalic = 1 << 1;
    public final static int fxUnderline = 1 << 2;
    public final static int fxBlink = 1 << 3;
    public final static int fxInverse = 1 << 4;
    public final static int fxInvisible = 1 << 5;
    public final static int ciForeground = 256;
    public final static int ciBackground = 257;
    public final static int ciCursor = 258;
    public final static int ciColorLength = ciCursor + 1;
    public final static int kNormalTextStyle = encode(ciForeground, ciBackground, fxNormal);

    public static int encode(int foreColor, int backColor, int effect) {
        return ((effect & 0x3f) << 18) | ((foreColor & 0x1ff) << 9) | (backColor & 0x1ff);
    }

    public static int decodeForeColor(int encodedColor) {
        return (encodedColor >> 9) & 0x1ff;
    }

    public static int decodeBackColor(int encodedColor) {
        return encodedColor & 0x1ff;
    }

    public static int decodeEffect(int encodedColor) {
        return (encodedColor >> 18) & 0x3f;
    }
}
