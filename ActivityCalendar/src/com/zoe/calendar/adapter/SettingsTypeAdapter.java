package com.zoe.calendar.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.SettingTypeItem;

public class SettingsTypeAdapter extends BaseAdapter<SettingTypeItem> {

	public SettingsTypeAdapter(Context context, List<SettingTypeItem> list) {
		super(context, list);
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_settings_type, parent, false);
		}
		SettingsTypeHolder holder = (SettingsTypeHolder) v.getTag();
		if (holder == null) {
			holder = new SettingsTypeHolder();
			holder.tvTitle = (TextView) v.findViewById(R.id.tvTitle);
			holder.ivChecked = (ImageView) v.findViewById(R.id.ivChecked);
			v.setTag(holder);
		}
		SettingTypeItem item = list.get(position);
		if (item != null) {
			holder.tvTitle.setText(item.title);
			holder.ivChecked
					.setImageResource(item.checked ? R.drawable.setting_checked
							: R.drawable.setting_unchecked);
		}
		return v;
	}

	@Override
	public String getValueText(SettingTypeItem item) {
		return "";
	}

}
