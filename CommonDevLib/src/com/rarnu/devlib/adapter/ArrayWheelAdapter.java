package com.rarnu.devlib.adapter;

import android.content.Context;

public class ArrayWheelAdapter<T> extends AbstractWheelTextAdapter {
    
    private T items[];

    public ArrayWheelAdapter(Context context, T items[]) {
        super(context);
        
        this.items = items;
    }
    
    @Override
    public CharSequence getItemText(int index) {
        if (index >= 0 && index < items.length) {
            T item = items[index];
            if (item instanceof CharSequence) {
                return (CharSequence) item;
            }
            return item.toString();
        }
        return null;
    }

    @Override
    public int getItemsCount() {
        return items.length;
    }
}
