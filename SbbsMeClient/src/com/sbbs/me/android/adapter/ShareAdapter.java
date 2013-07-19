package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.content.pm.ResolveInfo;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;

public class ShareAdapter extends BaseAdapter<ResolveInfo> {

	public ShareAdapter(Context context, List<ResolveInfo> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_share, parent, false);
		}
		ShareHolder holder = (ShareHolder) v.getTag();
		if (holder == null) {
			holder = new ShareHolder();
			holder.ivIcon = (ImageView) v.findViewById(R.id.ivIcon);
			v.setTag(holder);
		}
		ResolveInfo item = list.get(position);
		if (item != null) {
			holder.ivIcon.setImageDrawable(item.loadIcon(context
					.getPackageManager()));
		}
		return v;
	}

	@Override
	public String getValueText(ResolveInfo item) {
		return "";
	}

}
