package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.text.TextUtils.TruncateAt;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeArticleObject;

public class SbbsMeArticleMenuObjectAdapter extends
		BaseAdapter<SbbsMeArticleObject> {

	public SbbsMeArticleMenuObjectAdapter(Context context,
			List<SbbsMeArticleObject> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_article_menu_object, parent,
					false);
		}
		SbbsMeArticleMenuObjectHolder holder = (SbbsMeArticleMenuObjectHolder) v
				.getTag();
		if (holder == null) {
			holder = new SbbsMeArticleMenuObjectHolder();
			holder.tvText = (TextView) v.findViewById(R.id.tvText);
			holder.ivIcon = (ImageView) v.findViewById(R.id.ivIcon);
			v.setTag(holder);
		}
		SbbsMeArticleObject item = list.get(position);
		if (item != null) {
			switch (item.objType) {
			case 0:
				holder.ivIcon.setImageResource(R.drawable.type_url);
				holder.tvText.setEllipsize(TruncateAt.START);
				break;
			case 1:
				holder.ivIcon.setImageResource(R.drawable.type_image);
				holder.tvText.setEllipsize(TruncateAt.MIDDLE);
				break;
			}
			holder.tvText.setText(item.text);
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeArticleObject item) {
		return "";
	}

}
