package com.rarnu.devlib.component;

public class LockCircle {
    private int ox;
    private int oy;
    private float r;
    private Integer num;
    private boolean onTouch;

    public int getOx() {
        return ox;
    }

    public void setOx(int ox) {
        this.ox = ox;
    }

    public int getOy() {
        return oy;
    }

    public void setOy(int oy) {
        this.oy = oy;
    }

    public float getR() {
        return r;
    }

    public void setR(float r) {
        this.r = r;
    }

    public Integer getNum() {
        return num;
    }

    public void setNum(Integer num) {
        this.num = num;
    }

    public boolean isOnTouch() {
        return onTouch;
    }

    public void setOnTouch(boolean onTouch) {
        this.onTouch = onTouch;
    }

    public boolean isPointIn(int x, int y) {
        double distance = Math.sqrt((x - ox) * (x - ox) + (y - oy) * (y - oy));
        return distance < r;
    }
}
