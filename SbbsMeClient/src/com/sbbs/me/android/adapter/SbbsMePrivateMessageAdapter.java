package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.utils.ImageLoader;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMePrivateMessage;

public class SbbsMePrivateMessageAdapter extends
		BaseAdapter<SbbsMePrivateMessage> {

	private String myUserId;
	private String userAvatar;
	private String myAvatar;
	private ImageLoader iLoader;

	public SbbsMePrivateMessageAdapter(Context context,
			List<SbbsMePrivateMessage> list, String myUserId,
			String userAvatarUrl, String myAvatarPath) {
		super(context, list);
		this.myUserId = myUserId;
		this.userAvatar = userAvatarUrl;
		this.myAvatar = myAvatarPath;
		Log.e("SbbsMePrivateMessageAdapter", myAvatar);
		iLoader = new ImageLoader(context);
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
			if (item.FromUserId.equals(myUserId)) {
				// I sent
				holder.tvMessage.setGravity(Gravity.RIGHT
						| Gravity.CENTER_VERTICAL);
				holder.ivMyHead.setVisibility(View.VISIBLE);
				holder.ivUserHead.setVisibility(View.GONE);
				holder.ivMyHead.setImageBitmap(BitmapFactory
						.decodeFile(myAvatar));
			} else if (item.ToUserId.equals(myUserId)) {
				// I received
				holder.tvMessage.setGravity(Gravity.LEFT
						| Gravity.CENTER_VERTICAL);
				holder.ivMyHead.setVisibility(View.GONE);
				holder.ivUserHead.setVisibility(View.VISIBLE);
				iLoader.DisplayImage(userAvatar, holder.ivUserHead);
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
