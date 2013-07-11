package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeTag;

public class SbbsMeTagAdapter extends BaseAdapter<SbbsMeTag> {

	public SbbsMeTagAdapter(Context context, List<SbbsMeTag> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_tag, parent, false);
		}
		SbbsMeTagHolder holder = (SbbsMeTagHolder) v.getTag();
		if (holder == null) {
			holder = new SbbsMeTagHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			v.setTag(holder);
		}
		SbbsMeTag item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.Name);
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeTag item) {
		return "";
	}

}
