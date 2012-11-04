package com.rarnu.zoe.loving.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
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
			
			holder.setComponentSize(itemWidth);
			v.setTag(holder);
		}
		DataInfo item = list.get(position);
		if (item != null) {
			holder.tvItem1.setText(statToText(item.column, item.data1));
			holder.tvItem2.setText(statToText(item.column, item.data2));
			holder.tvItem3.setText(statToText(item.column, item.data3));
			
		}
		return v;
	}
	
	private String statToText(String column, int stat) {
		String ret = "";
		switch (stat) {
		case 1:
			ret = context.getString(R.string.lvl_1);
			break;
		case 2:
			ret = context.getString(R.string.lvl_2);
			break;
		case 3:
			ret = context.getString(R.string.lvl_3);
			break;
		}
		return ret;
	}

}
