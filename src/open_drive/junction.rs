use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JunctionLaneLink {
    pub from: i32,
    pub to: i32,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JunctionConnectionInformation {
    pub id: String,
    pub incoming_road: String,
    pub connecting_road: String,

    pub lane_links: BTreeSet<JunctionLaneLink>,
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum JunctionConnection {
    None(JunctionConnectionInformation),
    Start(JunctionConnectionInformation),
    End(JunctionConnectionInformation),
}

impl JunctionConnection {
    pub fn get_info(&self) -> &JunctionConnectionInformation {
        match self {
            Self::None(info) => info,
            Self::Start(info) => info,
            Self::End(info) => info,
        }
    }
}

#[derive(Debug)]
pub struct Junction {
    pub name: String,
    pub id: String,

    pub id_to_connection: BTreeMap<String, JunctionConnection>,
}

impl Junction {
    pub fn parse_junctions(node: &roxmltree::Node) -> HashMap<String, Junction> {
        let mut results = HashMap::new();
        for junction_node in node.children() {
            if !junction_node.has_tag_name("junction") {
                continue;
            }
            let junction_id = junction_node.attribute("id").unwrap_or("");
            let mut junction = Junction {
                name: junction_node.attribute("name").unwrap_or("").to_string(),
                id: junction_id.to_string(),
                id_to_connection: BTreeMap::new(),
            };

            for connection_node in junction_node.children() {
                if !connection_node.has_tag_name("connection") {
                    continue;
                }
                let junction_connection_id = connection_node.attribute("id").unwrap_or("");
                let mut connection_information = JunctionConnectionInformation {
                    id: junction_connection_id.to_string(),
                    connecting_road: connection_node
                        .attribute("connectingRoad")
                        .unwrap_or("")
                        .to_string(),
                    incoming_road: connection_node
                        .attribute("incomingRoad")
                        .unwrap_or("")
                        .to_string(),
                    lane_links: BTreeSet::new(),
                };
                for lane_link_node in connection_node.children() {
                    if !lane_link_node.has_tag_name("laneLink") {
                        continue;
                    }
                    connection_information.lane_links.insert(JunctionLaneLink {
                        from: lane_link_node
                            .attribute("from")
                            .unwrap_or("0")
                            .parse()
                            .unwrap(),
                        to: lane_link_node
                            .attribute("to")
                            .unwrap_or("0")
                            .parse()
                            .unwrap(),
                    });
                }
                let contact_point_str =
                    connection_node.attribute("contactPoint").unwrap_or("start");
                let junction_connection = if contact_point_str == "start" {
                    JunctionConnection::Start(connection_information)
                } else {
                    JunctionConnection::End(connection_information)
                };
                junction
                    .id_to_connection
                    .insert(junction_connection_id.to_string(), junction_connection);
            }
            results.insert(junction_id.to_string(), junction);
        }

        results
    }
}
