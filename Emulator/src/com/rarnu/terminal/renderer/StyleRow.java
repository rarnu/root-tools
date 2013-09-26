package com.rarnu.terminal.renderer;

public class StyleRow {
    private int mStyle;
    private int mColumns;
    private byte[] mData;

    public StyleRow(int style, int columns) {
        mStyle = style;
        mColumns = columns;
    }

    public void set(int column, int style) {
        if (style == mStyle && mData == null) {
            return;
        }
        ensureData();
        setStyle(column, style);
    }

    public int get(int column) {
        if (mData == null) {
            return mStyle;
        }
        return getStyle(column);
    }

    public boolean isSolidStyle() {
        return mData == null;
    }

    public int getSolidStyle() {
        if (mData != null) {
            throw new IllegalArgumentException("Not a solid style");
        }
        return mStyle;
    }

    public void copy(int start, StyleRow dst, int offset, int len) {
        if (mData == null && dst.mData == null && start == 0 && offset == 0 && len == mColumns) {
            dst.mStyle = mStyle;
            return;
        }

        ensureData();
        dst.ensureData();
        System.arraycopy(mData, 3 * start, dst.mData, 3 * offset, 3 * len);

    }

    public void ensureData() {
        if (mData == null) {
            allocate();
        }
    }

    private void allocate() {
        mData = new byte[3 * mColumns];
        for (int i = 0; i < mColumns; i++) {
            setStyle(i, mStyle);
        }
    }

    private int getStyle(int column) {
        int index = 3 * column;
        byte[] line = mData;
        return line[index] & 0xff | (line[index + 1] & 0xff) << 8 | (line[index + 2] & 0xff) << 16;
    }

    private void setStyle(int column, int value) {
        int index = 3 * column;
        byte[] line = mData;
        line[index] = (byte) (value & 0xff);
        line[index + 1] = (byte) ((value >> 8) & 0xff);
        line[index + 2] = (byte) ((value >> 16) & 0xff);
    }

}
