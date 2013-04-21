package com.yugioh.android.adapter;

import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.yugioh.android.R;

public class UpdateAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<String> list;

	public UpdateAdapter(LayoutInflater inflater, List<String> list) {
		this.inflater = inflater;
		this.list = list;
	}

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
		
		String item = list.get(position);
		View view;
		if (convertView == null) {
			view = inflater.inflate(R.layout.updateitem, parent, false);
		} else {
			view = convertView;
		}
		UpdateHolder holder = (UpdateHolder) view.getTag();
		if (holder == null) {
			holder = new UpdateHolder();
			holder.tvUpdateName = (TextView) view.findViewById(R.id.tvUpdateName);
			holder.pbDownloading = (ProgressBar) view.findViewById(R.id.pbDownloading);
			view.setTag(holder);
		}
		
		if (item != null) {
			holder.tvUpdateName.setText(item);
		}
		
		return view;
	}

}
