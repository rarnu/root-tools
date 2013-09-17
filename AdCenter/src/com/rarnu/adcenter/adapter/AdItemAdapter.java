package com.rarnu.adcenter.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.TextView;

import com.rarnu.adcenter.R;
import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.AdItem;
import com.rarnu.adcenter.utils.GifUtils;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.devlib.component.GifView;

public class AdItemAdapter extends BaseAdapter<AdItem> {

	private int itemHeight;
	private List<Boolean> listQuested;

	public AdItemAdapter(Context context, List<AdItem> list,
			List<Boolean> listQuested, int itemHeight) {
		super(context, list);
		this.itemHeight = itemHeight;
		this.listQuested = listQuested;
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
			holder.ivItem = (GifView) v.findViewById(R.id.ivItem);
			holder.tvItem = (TextView) v.findViewById(R.id.tvItem);
			holder.tvCost = (TextView) v.findViewById(R.id.tvCost);
			holder.tvCostValue = (TextView) v.findViewById(R.id.tvCostValue);
			v.setTag(holder);
		}
		AdItem item = list.get(position);
		if (item != null) {
			holder.tvItem.setText(item.title);
			GifUtils.loadGifImage(AdAPI.IMAGE_HOST + item.image_url,
					holder.ivItem);

			if (listQuested.get(position)) {
				holder.tvCost.setBackgroundResource(R.drawable.quested);
				holder.tvCostValue.setText("");
			} else {
				holder.tvCost.setBackgroundResource(R.drawable.money_bag);
				holder.tvCostValue.setText(String.valueOf(item.cost));
			}
		}
		return v;
	}

	@Override
	public String getValueText(AdItem item) {
		return "";
	}

}
