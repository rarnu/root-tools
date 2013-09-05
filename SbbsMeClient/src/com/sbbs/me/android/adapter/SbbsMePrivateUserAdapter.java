package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.utils.ImageLoader;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeInboxUser;

public class SbbsMePrivateUserAdapter extends BaseAdapter<SbbsMeInboxUser> {

	private ImageLoader iLoader;
	private List<Boolean> listNewMessage;

	public SbbsMePrivateUserAdapter(Context context,
			List<SbbsMeInboxUser> list, List<Boolean> listNewMessage) {
		super(context, list);
		this.listNewMessage = listNewMessage;
		iLoader = new ImageLoader(context);
	}
	
	public void setNewMessage(List<Boolean> listNewMessage) {
		this.listNewMessage = listNewMessage;
		this.notifyDataSetChanged();
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_private_user, parent, false);
		}
		SbbsMePrivateUserHolder holder = (SbbsMePrivateUserHolder) v.getTag();
		if (holder == null) {
			holder = new SbbsMePrivateUserHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			holder.ivHead = (ImageView) v.findViewById(R.id.ivHead);
			holder.tvNew = (TextView) v.findViewById(R.id.tvNew);
			v.setTag(holder);
		}
		SbbsMeInboxUser item = list.get(position);
		if (item != null) {
			iLoader.DisplayImage(item.Detail.AvatarURL, holder.ivHead);
			holder.tvName.setText(item.Detail.Name);
			holder.tvNew
					.setVisibility(listNewMessage.get(position) ? View.VISIBLE
							: View.GONE);
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeInboxUser item) {
		return "";
	}

}
