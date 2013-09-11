package com.rarnu.adcenter.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.adcenter.R;
import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.AdItem;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.utils.ImageLoader;
import com.rarnu.utils.UIUtils;

public class AdItemAdapter extends BaseAdapter<AdItem> {

	private int itemHeight;
	private ImageLoader imgLoader;
	private List<Boolean> listQuested;
	private RelativeLayout.LayoutParams alpQuested;
	private RelativeLayout.LayoutParams alpMoney;

	public AdItemAdapter(Context context, List<AdItem> list,
			List<Boolean> listQuested, int itemHeight) {
		super(context, list);
		this.itemHeight = itemHeight;
		imgLoader = new ImageLoader(context);
		this.listQuested = listQuested;
		alpQuested = new RelativeLayout.LayoutParams(UIUtils.dipToPx(16), UIUtils.dipToPx(16));
		alpMoney = new RelativeLayout.LayoutParams(UIUtils.dipToPx(24), UIUtils.dipToPx(24));
		alpQuested.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, RelativeLayout.TRUE);
		alpMoney.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, RelativeLayout.TRUE);
		alpQuested.rightMargin = UIUtils.dipToPx(2);
		alpQuested.topMargin = UIUtils.dipToPx(2);
		alpMoney.rightMargin = UIUtils.dipToPx(2);
		alpMoney.topMargin = UIUtils.dipToPx(2);
		
	}
	
	public void setNewQuestedList(List<Boolean> listQuested) {
		this.listQuested = listQuested;
		this.notifyDataSetChanged();
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_aditem, parent, false);
			v.setLayoutParams(new AbsListView.LayoutParams(itemHeight,
					itemHeight));
		}
		AdItemHolder holder = (AdItemHolder) v.getTag();
		if (holder == null) {
			holder = new AdItemHolder();
			holder.ivItem = (ImageView) v.findViewById(R.id.ivItem);
			holder.tvItem = (TextView) v.findViewById(R.id.tvItem);
			holder.tvCost = (TextView) v.findViewById(R.id.tvCost);
			v.setTag(holder);
		}
		AdItem item = list.get(position);
		if (item != null) {
			holder.tvItem.setText(item.title);
			imgLoader.DisplayImage(AdAPI.IMAGE_HOST + item.image_url,
					holder.ivItem);

			if (listQuested.get(position)) {
				holder.tvCost.setText("");
				holder.tvCost.setLayoutParams(alpQuested);
				holder.tvCost.setBackgroundResource(R.drawable.quested);
			} else {
				holder.tvCost.setText(String.valueOf(item.cost));
				holder.tvCost.setLayoutParams(alpMoney);
				holder.tvCost.setBackgroundResource(R.drawable.money_bag);
			}
		}
		return v;
	}

	@Override
	public String getValueText(AdItem item) {
		return "";
	}

}
