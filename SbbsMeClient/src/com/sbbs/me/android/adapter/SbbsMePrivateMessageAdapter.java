package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeUserLite;

public class SbbsMePrivateMessageAdapter extends BaseAdapter<SbbsMeUserLite> {

	public SbbsMePrivateMessageAdapter(Context context,
			List<SbbsMeUserLite> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_private_message, parent, false);
		}
		SbbsMePrivateMessageHolder holder = (SbbsMePrivateMessageHolder) v
				.getTag();
		if (holder == null) {
			holder = new SbbsMePrivateMessageHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			v.setTag(holder);
		}
		SbbsMeUserLite item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.Name);
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeUserLite item) {
		return "";
	}

}
