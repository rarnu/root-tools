package com.rarnu.findaround.adapter;

import java.util.List;

import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;

import com.rarnu.findaround.comp.GridPage4x4;

public class WelcomeAdapter extends BaseAdapter {

	// [region] field define

	private List<GridPage4x4> list;

	// [region] constructor
	public WelcomeAdapter(List<GridPage4x4> list) {
		this.list = list;
	}

	// [/region]

	// [region] adapter
	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		return list.get(position);
	}
	// [/region]
}
