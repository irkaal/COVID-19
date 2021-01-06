import { Component, OnInit } from '@angular/core';
import * as L from 'leaflet';

import { DataService } from '../data.service';

@Component({
  selector: 'app-map',
  templateUrl: './map.component.html',
  styleUrls: ['./map.component.scss'],
})
export class MapComponent implements OnInit {
  private _map!: L.Map;
  private _mapData!: {
    [Country_Region: string]: {
      [Field: string]: string[];
    };
  };

  constructor(private _dataService: DataService) {}

  ngOnInit(): void {
    this.initMap();
    this._dataService.getData().then((data) => {
      this._mapData = data;
      this.addMarkers();
    });
  }

  private initMap(): void {
    this._map = L.map('map', {
      center: [39.8282, -98.5795],
      zoom: 3,
    });

    const tiles = L.tileLayer(
      'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
      {
        maxZoom: 19,
        attribution:
          '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
      }
    );

    tiles.addTo(this._map);
  }

  private addMarkers(): void {
    console.log('Adding markers...');

    const countryRegionList = Object.keys(this._mapData).filter(
      (value: string) => value !== 'Worldwide'
    );

    countryRegionList.forEach((countryRegion: string) => {
      const crData = this._mapData[countryRegion];
      const lat = Number(crData.Latitude[crData.Latitude.length - 1]);
      const lon = Number(crData.Longitude[crData.Longitude.length - 1]);
      const confirmed = Number(crData.Confirmed[crData.Confirmed.length - 1]);
      L.circleMarker([lat, lon], {
        radius: Math.sqrt(Math.sqrt(confirmed)),
        weight: 0,
        color: 'rgba(222,55,0,1)',
      }).addTo(this._map);
    });
  }
}
