package com.snda.gyue.adapter;

import java.util.List;

import android.os.Handler;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.gyue.R;
import com.snda.gyue.classes.SettingsItem;
import com.snda.gyue.holder.SettingsItemHolder;

public class PreferenceAdapter extends BaseAdapter {

	private List<SettingsItem> list;
	private LayoutInflater inflater;
	private Handler handle;
	
	public PreferenceAdapter(LayoutInflater inflater, List<SettingsItem> list, Handler hMsg) {
		this.list = list;
		this.inflater = inflater;
		this.handle = hMsg;
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
	public View getView(int position, View convertView, final ViewGroup parent) {
		
		final SettingsItem item = list.get(position);
		
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.settings_item, parent, false);
		} else {
			v = convertView;
		}
		
		SettingsItemHolder holder = (SettingsItemHolder) v.getTag();
		if (holder == null) {
			holder= new SettingsItemHolder();
			holder.title = (TextView) v.findViewById(R.id.title);
			holder.summary = (TextView) v.findViewById(R.id.summary);
			holder.icon = (ImageView) v.findViewById(R.id.icon);
			holder.check = (CheckBox) v.findViewById(R.id.check);
			v.setTag(holder);
		}
		
		if (item != null) {
			holder.title.setText(item.getTitle());
			if (item.getSummary() == null || item.getSummary().equals("")) {
				holder.summary.setVisibility(View.GONE);
			} else {
				holder.summary.setText(item.getSummary());
				holder.summary.setVisibility(View.VISIBLE);
			}
			if (item.getIcon() == 0) {
				holder.icon.setVisibility(View.GONE);
			} else {
				holder.icon.setVisibility(View.VISIBLE);
				holder.icon.setBackgroundResource(item.getIcon());
			}
			holder.check.setVisibility(item.isCheckBox() ? View.VISIBLE: View.GONE);
			holder.check.setChecked(item.isChecked());
			holder.check.setOnCheckedChangeListener(new OnCheckedChangeListener() {
				@Override
				public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
					item.setChecked(isChecked);
					if (handle != null) {
						handle.sendEmptyMessage(101);
					}
				}
			});
		}
		
		return v;
	}

}
