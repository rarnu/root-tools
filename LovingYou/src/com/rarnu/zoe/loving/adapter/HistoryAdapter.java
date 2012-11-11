package com.rarnu.zoe.loving.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.common.DataInfo;

public class HistoryAdapter extends BaseAdapter {

	private Context context;
	private List<DataInfo> list;
	private LayoutInflater inflater;
	private int itemWidth;
	
	public HistoryAdapter(Context context, List<DataInfo> list, int itemWidth) {
		this.context = context;
		this.list = list;
		this.itemWidth = itemWidth;
		this.inflater = LayoutInflater.from(context);
	}
	
	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_history, parent, false);
		}
		HistoryHolder holder = (HistoryHolder) v.getTag();
		if (holder == null) {
			holder = new HistoryHolder();
			holder.tvItem1 = (TextView) v.findViewById(R.id.tvItem1);
			holder.tvItem2 = (TextView) v.findViewById(R.id.tvItem2);
			holder.tvItem3 = (TextView) v.findViewById(R.id.tvItem3);
			holder.imgItem1 = (ImageView) v.findViewById(R.id.imgItem1);
			holder.imgItem2 = (ImageView) v.findViewById(R.id.imgItem2);
			holder.imgItem3 = (ImageView) v.findViewById(R.id.imgItem3);
			
			holder.setComponentSize(itemWidth);
			v.setTag(holder);
		}
		DataInfo item = list.get(position);
		if (item != null) {
			
			holder.imgItem1.setVisibility(item.data1 == 1 ? View.VISIBLE: View.GONE);
			holder.imgItem2.setVisibility(item.data2 == 1 ? View.VISIBLE: View.GONE);
			holder.imgItem3.setVisibility(item.data3 == 1 ? View.VISIBLE: View.GONE);
			
			holder.tvItem1.setText(item.data1 == -1 ? "": context.getString(R.string.emotion2));
			holder.tvItem2.setText(item.data2 == -1 ? "": context.getString(R.string.emotion2));
			holder.tvItem3.setText(item.data3 == -1 ? "": context.getString(R.string.emotion2));

		}
		return v;
	}

}
