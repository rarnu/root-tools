package com.zoe.calendar.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;

public class ActivityAdapter extends BaseAdapter<ActivityItem> {

	public ActivityAdapter(Context context, List<ActivityItem> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_activity, parent, false);
		}
		ActivityHolder holder = (ActivityHolder) v.getTag();
		if (holder == null) {
			holder = new ActivityHolder();
			holder.tvLeftBLock = (TextView) v.findViewById(R.id.tvLeftBLock);
			holder.tvTime = (TextView) v.findViewById(R.id.tvTime);
			holder.tvTitle = (TextView) v.findViewById(R.id.tvTitle);

			v.setTag(holder);
		}
		ActivityItem item = list.get(position);
		if (item != null) {
			holder.tvLeftBLock
					.setBackgroundColor(position % 2 == 0 ? 0xff0099CC
							: 0xff666666);
			holder.tvTime.setText(item.getTimeRegion());
			holder.tvTime.setVisibility(holder.tvTime.getText().toString()
					.equals("") ? View.GONE : View.VISIBLE);
			holder.tvTitle.setText(item.title);
			holder.tvTime.setTextColor(position % 2 == 0 ? 0xff0099CC
					: 0xff666666);
			holder.tvTitle.setTextColor(position % 2 == 0 ? 0xff0099CC
					: 0xff666666);

		}
		return v;
	}

	@Override
	public String getValueText(ActivityItem item) {
		return "";
	}

}
