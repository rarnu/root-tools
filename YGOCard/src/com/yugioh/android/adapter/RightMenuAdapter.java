package com.yugioh.android.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseAdapter;
import com.yugioh.android.R;
import com.yugioh.android.classes.RightMenuItem;

public class RightMenuAdapter extends BaseAdapter<RightMenuItem> {

	private static final int[] fits = new int[] { R.drawable.c0, R.drawable.c1,
			R.drawable.c2, R.drawable.c3, R.drawable.c4, R.drawable.c5,
			R.drawable.c6, R.drawable.c7, R.drawable.c8, R.drawable.c9 };

	public RightMenuAdapter(Context context, List<RightMenuItem> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_menu_right, parent, false);
		}
		RightMenuHolder holder = (RightMenuHolder) v.getTag();
		if (holder == null) {
			holder = new RightMenuHolder();
			holder.tvMenuName = (TextView) v.findViewById(R.id.tvMenuName);
			holder.ivImg = (ImageView) v.findViewById(R.id.ivImg);
			v.setTag(holder);
		}

		RightMenuItem item = list.get(position);
		if (item != null) {
			holder.tvMenuName.setText(item.name);
			switch (item.type) {
			case 0:
				if (item.value == 0) {
					holder.ivImg.setImageDrawable(null);

				} else {
					holder.ivImg
							.setImageResource(android.R.drawable.ic_menu_upload);
				}
				break;
			case 1:
				holder.ivImg.setImageResource(fits[item.value]);
				break;
			}
		}

		return v;
	}

	@Override
	public String getValueText(RightMenuItem item) {
		return "";
	}

}
