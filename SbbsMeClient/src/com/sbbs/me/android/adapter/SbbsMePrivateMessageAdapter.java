package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMePrivateMessage;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.utils.Config;

public class SbbsMePrivateMessageAdapter extends
		BaseAdapter<SbbsMePrivateMessage> {

	private String myUserId;

	public SbbsMePrivateMessageAdapter(Context context,
			List<SbbsMePrivateMessage> list, String myUserId) {
		super(context, list);
		this.myUserId = myUserId;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_chat, parent, false);
		}
		SbbsMePrivateMessageHolder holder = (SbbsMePrivateMessageHolder) v
				.getTag();
		if (holder == null) {
			holder = new SbbsMePrivateMessageHolder();
			holder.tvMessage = (TextView) v.findViewById(R.id.tvMessage);
			holder.ivUserHead = (ImageView) v.findViewById(R.id.ivUserHead);
			holder.ivMyHead = (ImageView) v.findViewById(R.id.ivMyHead);
			v.setTag(holder);
		}
		SbbsMePrivateMessage item = list.get(position);
		if (item != null) {
			// TODO: set head
			if (item.FromUserId.equals(myUserId)) {
				// I sent
				holder.tvMessage.setGravity(Gravity.RIGHT
						| Gravity.CENTER_VERTICAL);
				holder.ivMyHead.setVisibility(View.VISIBLE);
				holder.ivUserHead.setVisibility(View.GONE);
				holder.ivMyHead.setImageBitmap(BitmapFactory
						.decodeFile(PathDefine.ROOT_PATH
								+ String.format("my%shead.jpg",
										Config.getAccountString(context))));
			} else if (item.ToUserId.equals(myUserId)) {
				// I received
				holder.tvMessage.setGravity(Gravity.LEFT
						| Gravity.CENTER_VERTICAL);
				holder.ivMyHead.setVisibility(View.GONE);
				holder.ivUserHead.setVisibility(View.VISIBLE);
				holder.ivUserHead.setImageBitmap(BitmapFactory
						.decodeFile(PathDefine.ROOT_PATH + item.FromUserId
								+ ".jpg"));
			}
			holder.tvMessage.setText(item.Body);
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMePrivateMessage item) {
		return "";
	}

}
