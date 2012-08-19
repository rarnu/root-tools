package com.rarnu.findaround.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.baidu.mapapi.MKRoute;
import com.rarnu.findaround.R;

public class RouteAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private MKRoute route;

	public RouteAdapter(LayoutInflater inflater, MKRoute route) {
		this.inflater = inflater;
		this.route = route;
	}

	@Override
	public int getCount() {
		return route.getNumSteps() - 1;
	}

	@Override
	public Object getItem(int position) {
		return route.getStep(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.route_item, parent, false);
		}
		RouteHolder holder = (RouteHolder) v.getTag();
		if (holder == null) {
			holder = new RouteHolder();
			holder.tvRouteItem = (TextView) v.findViewById(R.id.tvRouteItem);
		}

		holder.tvRouteItem.setText(route.getStep(position).getContent());

		return v;
	}

}
