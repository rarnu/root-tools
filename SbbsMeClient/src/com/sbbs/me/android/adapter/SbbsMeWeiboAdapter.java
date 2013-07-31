package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeWeibo;

public class SbbsMeWeiboAdapter extends BaseAdapter<SbbsMeWeibo> {

	private Handler hClick;

	public SbbsMeWeiboAdapter(Context context, List<SbbsMeWeibo> list,
			Handler hClick) {
		super(context, list);
		this.hClick = hClick;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_about_weibo, parent, false);
		}
		SbbsMeWeiboHolder holder = (SbbsMeWeiboHolder) v.getTag();
		if (holder == null) {
			holder = new SbbsMeWeiboHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			holder.tvDesc = (TextView) v.findViewById(R.id.tvDesc);
			holder.ivGithub = (ImageView) v.findViewById(R.id.ivGithub);
			holder.ivWeibo = (ImageView) v.findViewById(R.id.ivWeibo);
			v.setTag(holder);
		}

		final SbbsMeWeibo item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.name);
			holder.tvDesc.setText(item.desc);
			holder.ivGithub.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					if (hClick != null) {
						Message msg = new Message();
						msg.what = 1;
						msg.obj = item.github;
						hClick.sendMessage(msg);
					}

				}
			});
			holder.ivWeibo.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					Message msg = new Message();
					msg.what = 1;
					msg.obj = item.weibo;
					hClick.sendMessage(msg);
				}
			});
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeWeibo item) {
		return "";
	}

}
