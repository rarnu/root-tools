package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeMessage;

public class SbbsMeMessageAdapter extends BaseAdapter<SbbsMeMessage> {

	public SbbsMeMessageAdapter(Context context, List<SbbsMeMessage> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_message, parent, false);
		}
		SbbsMeMessageHolder holder = (SbbsMeMessageHolder) v.getTag();
		if (holder == null) {
			holder = new SbbsMeMessageHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			holder.tvSubject = (TextView) v.findViewById(R.id.tvSubject);
			holder.tvAction = (TextView) v.findViewById(R.id.tvAction);
			v.setTag(holder);
		}
		SbbsMeMessage item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.name);
			if (item.actionType == 0) {
				holder.tvAction.setText(R.string.msg_act_comment);
				holder.tvSubject.setText(item.postSubject);
				holder.tvSubject.setVisibility(View.VISIBLE);
			} else {
				holder.tvAction.setText(R.string.msg_act_follow);
				holder.tvSubject.setText("");
				holder.tvSubject.setVisibility(View.GONE);
			}
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeMessage item) {
		return "";
	}

}
