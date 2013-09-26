package com.rarnu.utils.common;

import java.util.ArrayList;
import java.util.List;

public class JsonNode {

    public String fieldName = "";
    public FieldType fieldType = FieldType.ftValue;
    public List<JsonNode> childs = null;
    public JsonNode subItemNode = null;

    public JsonNode(String name, FieldType type) {
        this.fieldName = name;
        this.fieldType = type;
        childs = new ArrayList<JsonNode>();
    }

    public JsonNode(String name, FieldType type, JsonNode node) {
        this(name, type);
        this.subItemNode = node;
    }

    public enum FieldType {
        ftValue, ftObject, ftList, ftMap
    }
}
