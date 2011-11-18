package com.snda.root.hosts;

import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.TextView;
import android.widget.CompoundButton.OnCheckedChangeListener;

public class HostListAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<HostItem> list;
	
	public HostListAdapter(LayoutInflater inflater, List<HostItem> list) {
		this.inflater = inflater;
		this.list = list;
	}
	
	public int getCount() {
		return list.size();
	}

	public Object getItem(int position) {
		return list.get(position);
	}

	public long getItemId(int position) {
		return position;
	}

	public View getView(int position, View convertView, ViewGroup parent) {
		final HostItem item = list.get(position);
		
		View view;
		if (convertView != null) {
			view = convertView;
		} else {
			view = inflater.inflate(R.layout.item, parent, false);
		}
		
		HostListAdapterHolder holder = (HostListAdapterHolder) view.getTag();
		if (holder == null) {
			holder = new HostListAdapterHolder();
			holder.chk = (CheckBox) view.findViewById(R.id.chk);
			holder.txt = (TextView) view.findViewById(R.id.txt);
			view.setTag(holder);
		}
		
		if (item != null) {
			holder.chk.setChecked(item.checked);
			holder.txt.setText(item.text);
			
			holder.chk.setOnCheckedChangeListener(new OnCheckedChangeListener() {
				public void onCheckedChanged(CompoundButton button, boolean checked) {
					item.checked = checked;
				}
			});
		}
		
		return view;
	}

}
