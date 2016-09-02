package com.rarnu.terminal.utils;

public class ColorScheme {
    private int foreColor;
    private int backColor;

    public ColorScheme(int foreColor, int backColor) {
        this.foreColor = foreColor;
        this.backColor = backColor;
    }

    public ColorScheme(int[] scheme) {
        if (scheme.length != 2) {
            throw new IllegalArgumentException();
        }

        this.foreColor = scheme[0];
        this.backColor = scheme[1];
    }

    public int getForeColor() {
        return foreColor;
    }

    public int getBackColor() {
        return backColor;
    }
}
