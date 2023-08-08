use osc2r2;
fn main() {
    let odr = osc2r2::open_drive::open_drive_parser::parse_open_drive("./Town04.xodr");
    println!("{:?}", odr);

    let mesh = odr.get_road_network_mesh();
    println!("{:?}", mesh);
}
