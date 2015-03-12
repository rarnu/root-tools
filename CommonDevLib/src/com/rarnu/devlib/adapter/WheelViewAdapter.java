package com.rarnu.devlib.adapter;

import android.database.DataSetObserver;
import android.view.View;
import android.view.ViewGroup;

public interface WheelViewAdapter {

	public int getItemsCount();

	public View getItem(int index, View convertView, ViewGroup parent);

	public View getEmptyItem(View convertView, ViewGroup parent);

	public void registerDataSetObserver(DataSetObserver observer);

	void unregisterDataSetObserver (DataSetObserver observer);
}
